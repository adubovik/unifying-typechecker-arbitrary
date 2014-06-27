{-# language
   RecordWildCards
 , MultiParamTypeClasses
 #-}

module Exp where

import FromList
import DataConstructorM
import Language

data Exp = Add Exp Exp
         | Lit Int
         | Eq Exp Exp
  deriving Show

instance FromList m => Language (DCM m) Exp where
  add = addExp
  eq  = eqExp
  lit = litExp

addExp :: FromList m => DCM m Exp (Exp,(Exp,()))
addExp = DCM {..}
  where
    unapply = \e -> case e of
      Add x y -> fromList [(x,(y,()))]
      _       -> fromList []

    apply = \e -> case e of
      (x,(y,())) -> fromList [Add x y]

eqExp :: FromList m => DCM m Exp (Exp,(Exp,()))
eqExp = DCM {..}
  where
    unapply = \e -> case e of
      Eq x y -> fromList [(x,(y,()))]
      _      -> fromList []

    apply = \e -> case e of
      (x,(y,())) -> fromList [Eq x y]

litExp :: FromList m => DCM m Exp ()
litExp = DCM {..}
  where
    unapply = \e -> case e of
      Lit _ -> fromList [()]
      _     -> fromList []

    apply = \e -> case e of
      () -> fromList [ Lit i | i <- [0..0] ]



