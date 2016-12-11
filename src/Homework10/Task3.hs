{-# LANGUAGE TemplateHaskell #-}

module Homework10.Task3 where

import           Control.Lens        (view, _1)
import           Data.List           (intercalate)
import           Language.Haskell.TH

showDname :: Name -> Q Exp
showDname = pure . LitE . stringL . nameBase

showPretty :: Name -> Q [Dec]
showPretty name = do
  TyConI (DataD _ _ _ _ [RecC dname fields] _) <- reify name
  let names = map (view _1) fields
      showField :: Name -> Q Exp
      showField name = let s = nameBase name
                       in [|\x -> "  " ++ s ++ " = " ++ show ($(varE name) x)|]
      showFields :: Q Exp
      showFields = listE $ map showField names
  [d|instance Show $(conT name) where
           show x = $(showDname dname) ++ " {\n" ++ intercalate ",\n"  (map ($ x) $showFields) ++ "\n}"|]

