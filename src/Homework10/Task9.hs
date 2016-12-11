{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Homework10.Task9
       ( runWalker
       ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Control.Monad.State    (StateT, evalStateT)
import           System.IO              (hFlush, stdout)

import           Homework10.Task6
import           Homework10.Task7
import           Homework10.Task8

data WalkerState = WalkerState
  { _dir       :: [AnnotatedFS]
  , _filesSeen :: Int
  , _dirsSeen  :: Int
  }

makeLenses ''WalkerState

type Walker = StateT WalkerState (ReaderT FS IO)
data Command = Cd FilePath
             | Up
             | Nop

parseCmd :: String -> Maybe Command
parseCmd (words -> [])         = Just Nop
parseCmd (words -> ["cd", fn]) = Just $ Cd fn
parseCmd (words -> ["up"])     = Just Up
parseCmd _                     = Nothing

initialState :: FS -> WalkerState
initialState fs =
  let initFilesSeen = lengthOf files fs
      initDirsSeen = lengthOf dirs fs
  in WalkerState [AFS "" fs] initFilesSeen initDirsSeen

reportState :: Walker ()
reportState = do
  curPath <- use $ dir . to head . getPath
  curFiles <- use filesSeen
  curDirs <- use dirsSeen
  rootDir <- view name
  liftIO $ do
    putStrLn $ "You are in \"" ++ curPath ++ "\""
    putStrLn $ "Files from root \"" ++ rootDir ++ "\": " ++ show curFiles
    putStrLn $ "Directories from root \"" ++ rootDir ++ "\": " ++ show curDirs

runCommands :: Walker ()
runCommands = do
  reportState
  liftIO $ putStr "> " >> hFlush stdout
  line <- liftIO getLine
  let ecmd = parseCmd line
  case ecmd of
    Nothing      -> liftIO $ putStrLn $ "Invalid command: " ++ line
    Just Nop     -> pure ()
    Just Up      -> arise
    Just (Cd dr) -> dive dr
  runCommands

dive :: FilePath -> Walker ()
dive dr = do
  validDir <- use $ dir . to head . hasChild dr
  if not validDir
    then liftIO $ putStrLn $ "No such file or directory: " ++ dr
    else do
    ndir <- use $ dir . to head . move dr
    filesSeen += lengthOf files ndir
    dirsSeen += lengthOf dirs ndir
    dir %= (ndir:)

arise :: Walker ()
arise = do
  curFiles <- use $ count $ dir . to head . files
  curDirs <- use $ count $ dir . to head . dirs
  filesSeen -= curFiles
  dirsSeen -= curDirs
  dir %= tail

runWalker :: FS -> IO ()
runWalker fs = flip runReaderT fs $ evalStateT runCommands $ initialState fs

