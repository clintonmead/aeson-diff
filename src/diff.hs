{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Exception (bracket)
import           Data.Aeson.Codec (encode, decode, AesonFormat(..))
import           Data.Aeson (Value)
import           Data.Aeson.Diff (Config(Config), diff')
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import           Options.Applicative (fullDesc, info, execParser, helper, metavar, progDesc, argument, help, value, long, option, short, switch)
import           Options.Applicative.Types (Parser, readerAsk)
import           System.IO (Handle, IOMode(ReadMode, WriteMode), hClose, openFile, stdin, stdout)

type File = Maybe FilePath

type Handle' = (Handle, Maybe AesonFormat, File)

-- | Command-line options.
data DiffOptions = DiffOptions
    { optionTst  :: Bool
    , optionOut  :: File
    , optionFrom :: File
    , optionTo   :: File
    , optionYaml :: Bool
    }

data Configuration = Configuration
    { cfgOptions :: DiffOptions
    , cfgTst  :: Bool
    , cfgOut  :: Handle
    , cfgFrom :: Handle
    , cfgTo   :: Handle
    }

optionParser :: Parser DiffOptions
optionParser = DiffOptions
    <$> switch
        (  long "test-before-remove"
        <> short 'T'
        <> help "Include a test before each remove."
        )
    <*> option fileP
        (  long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Write patch to file OUTPUT."
        <> value Nothing
        )
    <*> argument fileP
        (  metavar "FROM"
        )
    <*> argument fileP
        (  metavar "TO"
        )
    <*> switch
        (  long "yaml"
        <> help "Use yaml decoding and encoding."
        )
  where
    fileP = do
        s <- readerAsk
        return $ case s of
            "-" -> Nothing
            _ -> Just s

jsonFile :: Handle' -> IO Value
jsonFile (fp, mformat, mfilepath) = do
    s <- BS.hGetContents fp
    case decode mformat mfilepath (BSL.fromStrict s) of
        Nothing -> error "Could not parse as JSON"
        Just v -> return v

run :: DiffOptions -> IO ()
run opt = bracket (load opt) close process
  where
    openr :: Maybe FilePath -> IO Handle
    openr Nothing = return stdin
    openr (Just p) = openFile p ReadMode

    openw :: Maybe FilePath -> IO Handle
    openw Nothing = return stdout
    openw (Just p) = openFile p WriteMode

    load :: DiffOptions -> IO Configuration
    load options@DiffOptions{..} =
        Configuration
            <$> pure  options
            <*> pure  optionTst
            <*> openw optionOut
            <*> openr optionFrom
            <*> openr optionTo

    close :: Configuration -> IO ()
    close Configuration{..} = do
        hClose cfgOut
        hClose cfgFrom
        hClose cfgTo

process :: Configuration -> IO ()
process Configuration{..} = do
    let mformat = if optionYaml cfgOptions then Just AesonYAML else Nothing
    json_from <- jsonFile (cfgFrom, mformat, optionFrom cfgOptions)
    json_to <- jsonFile (cfgTo, mformat, optionTo cfgOptions)
    let c = Config cfgTst
    let p = diff' c json_from json_to
    BS.hPutStrLn cfgOut $ BSL.toStrict (encode mformat (optionOut cfgOptions) p)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> optionParser)
     (  fullDesc
     <> progDesc "Generate a patch between two JSON documents.")
