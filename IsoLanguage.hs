module IsoLanguage where

import Prelude hiding(exp)
import Control.Monad

import FromList
import Exp
import Type
import qualified Language as L
import BiIsoM

lit :: (Monad m, FromList m) =>
       BiIsoM m Exp () () Type
lit = mkBiIsoM L.lit L.lit

add :: (Monad m, FromList m) =>
       BiIsoM m Exp (Exp, (Exp, ())) (Type, (Type, ())) Type
add = mkBiIsoM L.add L.add

eq :: (Monad m, FromList m) =>
      BiIsoM m Exp (Exp, (Exp, ())) (Type, (Type, ())) Type
eq = mkBiIsoM L.eq L.eq

exp :: (MonadPlus m, Monad m, FromList m) =>
       BiIsoM m Exp () () Type
exp =     lit
      <|> (add <*> exp <*> exp)
      <|> (eq  <*> exp <*> exp)
