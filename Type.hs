{-# language
   RecordWildCards
 , MultiParamTypeClasses
 #-}

module Type where

import FromList
import DataConstructorM
import Language

data Type = TInt
          | TBool
  deriving (Eq,Show)

instance FromList m => Language (DCM m) Type where
  add = addType
  eq  = eqType
  lit = litType

addType :: FromList m => DCM m Type (Type,(Type,()))
addType = DCM {..}
  where
    unapply = \tt -> case tt of
      TInt -> fromList [(TInt,(TInt,()))]
      _    -> fromList []

    apply = \tt -> case tt of
      (TInt,(TInt,())) -> fromList [TInt]
      _                -> fromList []

eqType :: FromList m => DCM m Type (Type,(Type,()))
eqType = DCM {..}
  where
    unapply = \t -> case t of
      TBool -> fromList [ (TInt,(TInt,()))
                        , (TBool,(TBool,()))
                        ]
      _     -> fromList []

    apply = \tt -> case tt of
      (x,(y,())) | x == y -> fromList [TBool]
      _                   -> fromList []

litType :: FromList m => DCM m Type ()
litType = DCM {..}
  where
    unapply = \t -> case t of
      TInt -> fromList [()]
      _    -> fromList []

    apply = \t -> case t of
      () -> fromList [TInt]
