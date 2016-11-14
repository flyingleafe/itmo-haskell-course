module Main where

import           Data.String           (IsString (..))
import           Data.Text             (Text)
import           Homework7             (ArithVal, VarName, fromParsec, makeMapping,
                                        parseArith, runEval, varPair)
import           Options.Applicative   hiding (strOption)
import           Text.Megaparsec.Error (parseErrorPretty)

data Action = Print
            | Eval
            deriving (Show, Read)

data ExprOptions = EO
  { exprAction :: !Action
  , expr       :: !Text
  , vars       :: ![(VarName, ArithVal)]}

-- | Honestly stolen from serokell-util
fromStr :: IsString s => ReadM s
fromStr = fromString <$> str

strOption :: IsString s => Mod OptionFields s -> Parser s
strOption = option fromStr

argsParser :: Parser ExprOptions
argsParser = EO
  <$> option auto (long "exprAction"
                <> short 'a'
                <> help "Action to perform: print AST or evaluate")
  <*> strOption (long "expr"
              <> short 'e'
              <> help "Expression to parse")
  <*> many (option (fromParsec varPair) $
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
    Left err -> putStrLn $ "Error while parsing expression: " ++ parseErrorPretty err
    Right ast -> case exprAction of
      Print -> print ast
      Eval  -> print $ runEval (makeMapping vars) ast
