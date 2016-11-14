module Homework7.Task2
       ( VarName
       , ArithVal
       , runEval
       , parseArith
       , makeMapping
       , emptyMap
       , fromParsec
       , varPair
       ) where

import           Control.Applicative   ((<|>))
import           Control.Monad.Reader  (MonadReader (..), Reader, runReader)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Scientific       (toRealFloat)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Options.Applicative   (ReadM, eitherReader)
import           Text.Megaparsec       (Dec, ParseError (..), alphaNumChar, char,
                                        letterChar, many, parse, space)
import           Text.Megaparsec.Error (parseErrorPretty)
import           Text.Megaparsec.Expr  (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

-- | Datatypes
type VarName = Text
type ArithVal = Double
type VarMapping = VarName -> ArithVal

emptyMap :: VarMapping
emptyMap x = error $ T.unpack $ "Undefined variable `" <> x <> "`"

makeMapping :: [(VarName, ArithVal)] -> VarMapping
makeMapping list x = fromMaybe (emptyMap x) $ lookup x list

assignVar :: VarName -> ArithVal -> VarMapping -> VarMapping
assignVar x v mp = \y -> if x == y then v else mp y

data Arith = Lit ArithVal
           | Var VarName
           | Neg Arith
           | Add Arith Arith
           | Sub Arith Arith
           | Mul Arith Arith
           | Div Arith Arith
           | Pow Arith Arith
           | Assign VarName ArithVal Arith
           deriving (Show, Eq)

-- | Evaluation
type Eval = Reader VarMapping ArithVal

eval :: Arith -> Eval
eval (Lit v)        = pure v
eval (Var x)        = ask <*> pure x
eval (Neg a)        = negate <$> eval a
eval (Add a b)      = (+) <$> eval a <*> eval b
eval (Sub a b)      = (-) <$> eval a <*> eval b
eval (Mul a b)      = (*) <$> eval a <*> eval b
eval (Div a b)      = (/) <$> eval a <*> eval b
eval (Pow a b)      = (**) <$> eval a <*> eval b
eval (Assign x v a) = local (assignVar x v) $ eval a

runEval :: VarMapping -> Arith -> ArithVal
runEval mapping expr = runReader (eval expr) mapping

-- | Parsing
symbol :: Text -> Parser Text
symbol = fmap T.pack . L.symbol space . T.unpack

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

number :: Parser ArithVal
number = toRealFloat <$> lexeme L.number

signed :: Parser ArithVal
signed = toRealFloat <$> L.signed space L.number

identifier :: Parser VarName
identifier = lexeme $ T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

arithTerm :: Parser Arith
arithTerm = (Lit <$> number)
        <|> (Var <$> identifier)
        <|> parens arith

arith :: Parser Arith
arith = makeExprParser arithTerm arithTable
  where arithTable = [ [ prefix "-" Neg
                       , prefix "+" id ]
                     , [ binaryR "^" Pow ]
                     , [ binaryL "*" Mul
                       , binaryL "/" Div ]
                     , [ binaryL "+" Add
                       , binaryL "-" Sub ]
                     ]
        binaryL name f = InfixL (f <$ symbol name)
        binaryR name f = InfixR (f <$ symbol name)
        prefix name f = Prefix (f <$ symbol name)

parseArith :: Text -> Either (ParseError Char Dec) Arith
parseArith expr = parse arith "" expr

-- | Options helper
varPair :: Parser (VarName, ArithVal)
varPair = parens $ (,) <$> (identifier <* lexeme (char ',')) <*> signed

fromParsec :: Parser a -> ReadM a
fromParsec parser = eitherReader $ either (Left . parseErrorPretty) Right . parse parser "<CLI options>" . T.pack
