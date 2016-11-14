module Main where

import           Data.Text           (Text, pack)
import           Homework7           (ArithVal, VarName, makeMapping, parseArith, runEval)
import           Options.Applicative

data Action = Print
            | Eval
            deriving (Show, Read)

data ExprOptions = EO
  { exprAction :: !Action
  , expr       :: !Text
  , vars       :: ![(VarName, ArithVal)]}

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap pack . strOption

argsParser :: Parser ExprOptions
argsParser = EO
  <$> option auto (long "exprAction"
                <> short 'a'
                <> help "Action to perform: print AST or evaluate")
  <*> textOption (long "expr"
                <> short 'e'
                <> help "Expression to parse")
  <*> many (option auto $
            long "var"
         <> short 'v'
         <> help "Free variables values")

argsInfo :: ParserInfo ExprOptions
argsInfo = info (helper <*> argsParser) $
  fullDesc <> progDesc "Expressiong parser and evaluator"

main :: IO ()
main = do
  EO {..} <- execParser argsInfo
  let east = parseArith expr
  case east of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> case exprAction of
      Print -> print ast
      Eval  -> print $ runEval (makeMapping vars) ast
