{-# LANGUAGE NoImplicitPrelude #-}

module Homework7.Task1
       ( runConfigurator
       , parseFile
       ) where

import           Control.Monad        (forM_)
import           Data.IORef           (IORef, modifyIORef, newIORef, readIORef,
                                       writeIORef)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.IO         (getLine, hGetContents, hPutStrLn, putStr, putStrLn)
import           Prelude              hiding (getLine, putStr, putStrLn)
import           System.IO            (IOMode (..), hClose, hFlush, openFile, stdout)
import           System.IO.Unsafe     (unsafePerformIO)
import           Text.Megaparsec
import           Text.Megaparsec.Text (Parser)

type Config = Map Text Text
type Directive = (Text, Text)
type ConfigHistory = NonEmpty Config

-- | IORef
configHistory :: IORef ConfigHistory
configHistory = unsafePerformIO $ newIORef (M.empty :| [])

configFile :: IORef FilePath
configFile = unsafePerformIO $ newIORef ""

-- | Config parser
config :: Parser Config
config = M.fromList <$> many (directive <* eol)

directive :: Parser Directive
directive = (,) <$> (property <* char '=') <*> value

property :: Parser Text
property = T.pack <$> some (alphaNumChar <|> oneOf ("_-" :: String))

value :: Parser Text
value = T.pack <$> some printChar

-- | Config operations
update :: Directive -> ConfigHistory -> ConfigHistory
update (prop, val) (cfg :| rest) = M.insert prop val cfg :| (cfg : rest)

undo :: ConfigHistory -> (Bool, ConfigHistory)
undo his@(_ :| []) = (False, his)
undo (_ :| (x:xs)) = (True, x :| xs)

-- | Commands
cmdMap :: Map Text (IO ())
cmdMap = M.fromList
  [ ("q", cmdQuit)
  , ("w", cmdSave)
  , ("u", cmdUndo)
  , ("p", cmdPreview)
  , ("h", cmdHelp)
  ]

cmdQuit :: IO ()
cmdQuit = pure ()

cmdSave :: IO ()
cmdSave = do
  file <- readIORef configFile
  cfgs <- readIORef configHistory
  handle <- openFile file WriteMode
  let lastVer = M.toList $ NE.head cfgs

  forM_ lastVer $ \(prop, val) ->
    hPutStrLn handle $ prop <> "=" <> val

  putStrLn $ "Saved to " <> T.pack file

  hClose handle
  awaitCommand

cmdPreview :: IO ()
cmdPreview = do
  cfgs <- readIORef configHistory
  let lastVer = M.toList $ NE.head cfgs

  forM_ lastVer $ \(prop, val) ->
    putStrLn $ prop <> "=" <> val

  awaitCommand

cmdUndo :: IO ()
cmdUndo = do
  cfgs <- readIORef configHistory
  let (undone, cfgs') = undo cfgs
  writeIORef configHistory cfgs'

  if not undone
    then putStrLn "No actions to undo"
    else putStrLn $ "Change undone"

  awaitCommand

cmdHelp :: IO ()
cmdHelp = putStrLn helpMsg >> awaitCommand
  where helpMsg = "Commands:\
                  \  <prop>=<val>  -- assign value <val> to property <prop>\n\
                  \  :w   -- save changes to file\n\
                  \  :q   -- quit (without saving)\n\
                  \  :u   -- undo last change\n\
                  \  :p   -- preview current version of config\n\
                  \  :h   -- show this message"

-- | Working functions
parseFile :: FilePath -> IO ConfigHistory
parseFile file = do
  text <- hGetContents =<< openFile file ReadMode
  case parse config file text of
    Left err  -> fail $ "parseFile: " ++ show err
    Right res -> return $ res :| []

editAction :: Text -> IO ()
editAction action = case parse directive "" action of
  Left _ -> putStrLn ("Invalid property declaration: " <> action)
  Right res@(prop, val) -> do
    modifyIORef configHistory $ update res
    putStrLn $ "Assigned value " <> val <> " to property " <> prop

runCommand :: Text -> IO ()
runCommand cmd = fromMaybe notFound $ M.lookup cmd cmdMap
  where notFound = putStrLn ("Unknown command `:" <> cmd <> "`") >> awaitCommand

awaitCommand :: IO ()
awaitCommand = do
  putStr "configurator> "
  hFlush stdout
  cmd <- getLine
  if T.null cmd
    then awaitCommand
    else do
    if T.head cmd == ':'
      then runCommand (T.tail cmd)
      else editAction cmd >> awaitCommand

runConfigurator :: FilePath -> IO ()
runConfigurator file = do
  putStrLn "Welcome to wow-so-convenient interactive configurator!"
  putStrLn "Use `:h` command for help."

  -- touch the file
  h <- openFile file ReadWriteMode
  hClose h

  writeIORef configFile file
  parseFile file >>= writeIORef configHistory
  awaitCommand
