module Homework6.Task2
       ( Arith (..)
       , runEval
       , testExpr
       ) where

import           Control.Monad.Reader (MonadReader (..), Reader, runReader)

type VarName = String
type ArithVal = Double
type VarMapping = VarName -> ArithVal

emptyMap :: VarMapping
emptyMap _ = error "Undefined variable"

assignVar :: VarName -> ArithVal -> VarMapping -> VarMapping
assignVar x v mp = \y -> if x == y then v else mp y

data Arith = Lit ArithVal
           | Var VarName
           | Add Arith Arith
           | Mul Arith Arith
           | Assign VarName ArithVal Arith

type Eval = Reader VarMapping ArithVal

eval :: Arith -> Eval
eval (Lit v)        = pure v
eval (Var x)        = ask <*> pure x
eval (Add a b)      = (+) <$> eval a <*> eval b
eval (Mul a b)      = (*) <$> eval a <*> eval b
eval (Assign x v a) = local (assignVar x v) $ eval a

runEval :: Arith -> ArithVal
runEval expr = runReader (eval expr) emptyMap

testExpr :: Arith
testExpr = "x" `Assign` 1 $ Var "x" `Add` (Lit 3 `Mul` ("x" `Assign` 2 $ Var "x"))
