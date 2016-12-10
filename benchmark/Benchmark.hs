-- import qualified Bench.Homework3.Task3Bench as Task3Bench
import qualified Bench.Homework9.Task1Bench as Task1Bench

import           System.IO

main :: IO ()
main = do
  hSetEncoding stdout utf8
  Task1Bench.main
