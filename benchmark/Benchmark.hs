import qualified Bench.Homework3.Task3Bench as Task3Bench

import           System.IO                  (Handle, hGetEncoding, hSetEncoding,
                                             mkTextEncoding, stderr, stdin,
                                             stdout)

makeSafe :: Handle -> IO ()
makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> mkTextEncoding (takeWhile (/= '/') $ show ce ++ "//TRANSLIT") >>=
      hSetEncoding h

main :: IO ()
main = do
  mapM_ makeSafe [stdout, stdin, stderr]
  Task3Bench.main
