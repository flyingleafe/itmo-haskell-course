import qualified Bench.Homework3.Task3Bench as Task3Bench

import           System.IO

main :: IO ()
main = do
  hSetEncoding stdout utf8
  Task3Bench.main
