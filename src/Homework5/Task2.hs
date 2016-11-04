module Homework5.Task2 where

import           Control.Applicative (Alternative (..))
import           Data.Bifunctor      (first)
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

-- Exercise 1. Functor instance
instance Functor Parser where
  fmap f p = Parser $ \s -> first f <$> runParser p s

-- Exercise 2. Applicative instance
instance Applicative Parser where
  pure x = Parser $ \s -> return (x, s)
  pf <*> px = Parser $ \s -> do
    (f, s') <- runParser pf s
    runParser (f <$> px) s'

-- Exercise 4. Alternative instance
instance Alternative Parser where
  empty = Parser $ const empty
  pa <|> pb = Parser $ \s -> runParser pa s <|> runParser pb s

-- low-level parsers
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (c:cs)
          | p c = Just (c, cs)
          | otherwise = Nothing

posInt :: Parser Integer
posInt = Parser f
  where f xs
          | null ns   = Nothing
          | otherwise = Just (read ns, rest)
          where (ns, rest) = span isDigit xs

-- Exercise 3. derived parsers
char :: Char -> Parser Char
char c = satisfy (== c)

ignore :: Parser a -> Parser ()
ignore p = p *> pure ()

space :: Parser Char
space = satisfy isSpace

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = ignore abParser

intPair :: Parser [Integer]
intPair = (\a b -> [a, b]) <$> (posInt <* space) <*> posInt

-- Exercise 5. intOrUppercase
intOrUppercase :: Parser ()
intOrUppercase = ignore posInt <|> ignore (satisfy isUpper)

-- Exercise 6. zeroOrMore & oneOrMore
zeroOrMore, oneOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- Exercise 7. spaces & ident
spaces :: Parser String
spaces = zeroOrMore space

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- Exercise 8. SExp
type Ident = String

data Atom = N Integer | I Ident deriving Show
data SExpr = A Atom | Comb [SExpr] deriving Show

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

parseSExpr :: Parser SExpr
parseSExpr = lexeme $ A <$> atom
             <|> Comb <$> (char '(' *> zeroOrMore parseSExpr <* lexeme (char ')'))

atom :: Parser Atom
atom = N <$> posInt <|> I <$> ident

-- Convenience function (for manual testing)
parseS :: String -> Maybe SExpr
parseS s = fst <$> runParser parseSExpr s
