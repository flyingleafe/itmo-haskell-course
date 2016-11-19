module Homework8.Task3
       ( runConfigurator
       ) where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (MonadReader (..), ReaderT (..))
import           Control.Monad.State    (MonadState (..), StateT (..), evalStateT, gets,
                                         modify)
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NE
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.IO           (getLine, hGetContents, hPutStrLn, putStr,
                                         putStrLn)
import           Prelude                hiding (getLine, putStr, putStrLn)
import           System.IO              (IOMode (..), hClose, hFlush, openFile, stdout)
import           Text.Megaparsec
import           Text.Megaparsec.Text   (Parser)

type Config = Map Text Text
type Directive = (Text, Text)
type ConfigHistory = NonEmpty Config

type Configurator = ReaderT FilePath (StateT ConfigHistory IO)

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
cmdMap :: Map Text (Configurator ())
cmdMap = M.fromList
  [ ("q", cmdQuit)
  , ("w", cmdSave)
  , ("u", cmdUndo)
  , ("p", cmdPreview)
  , ("h", cmdHelp)
  ]

cmdQuit :: Configurator ()
cmdQuit = pure ()

cmdSave :: Configurator ()
cmdSave = do
  file <- ask
  cfg <- gets NE.head
  handle <- liftIO $ openFile file WriteMode

  forM_ (M.toList cfg) $ \(prop, val) ->
    liftIO $ hPutStrLn handle $ prop <> "=" <> val

  liftIO $ putStrLn $ "Saved to " <> T.pack file

  liftIO $ hClose handle
  awaitCommand

cmdPreview :: Configurator ()
cmdPreview = do
  cfg <- gets NE.head

  forM_ (M.toList cfg) $ \(prop, val) ->
    liftIO $ putStrLn $ prop <> "=" <> val

  awaitCommand

cmdUndo :: Configurator ()
cmdUndo = do
  cfgs <- get
  let (undone, cfgs') = undo cfgs
  put cfgs'

  liftIO $ if not undone
    then putStrLn "No actions to undo"
    else putStrLn $ "Change undone"

  awaitCommand

cmdHelp :: Configurator ()
cmdHelp = liftIO (putStrLn helpMsg) >> awaitCommand
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

editAction :: Text -> Configurator ()
editAction action = case parse directive "" action of
  Left _ -> liftIO $ putStrLn ("Invalid property declaration: " <> action)
  Right res@(prop, val) -> do
    modify $ update res
    liftIO $ putStrLn $ "Assigned value " <> val <> " to property " <> prop

runCommand :: Text -> Configurator ()
runCommand cmd = fromMaybe notFound $ M.lookup cmd cmdMap
  where notFound = liftIO (putStrLn $ "Unknown command `:" <> cmd <> "`") >> awaitCommand

awaitCommand :: Configurator ()
awaitCommand = do
  liftIO $ putStr "configurator> "
  liftIO $ hFlush stdout
  cmd <- liftIO getLine
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

  cfgs <- parseFile file
  flip evalStateT cfgs $ flip runReaderT file $ awaitCommand
