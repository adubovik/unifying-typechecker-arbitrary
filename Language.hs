module Language where

import Prelude hiding(exp)
import Control.Monad

import BiIsoM
import FromList

import Exp
import Exp.DataConstructors

import Type
import Type.DataConstructors

lit :: (Monad m, FromList m) =>
       BiIsoM m Exp () () Type
lit = mkBiIsoM litExp litType

add :: (Monad m, FromList m) =>
       BiIsoM m Exp (Exp, (Exp, ())) (Type, (Type, ())) Type
add = mkBiIsoM addExp addType

eq :: (Monad m, FromList m) =>
      BiIsoM m Exp (Exp, (Exp, ())) (Type, (Type, ())) Type
eq = mkBiIsoM eqExp eqType

exp :: (MonadPlus m, Monad m, FromList m) =>
       BiIsoM m Exp () () Type
exp =     lit
      <|> (add <*> exp <*> exp)
      <|> (eq  <*> exp <*> exp)
