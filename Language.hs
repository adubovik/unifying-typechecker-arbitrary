{-# language
   MultiParamTypeClasses
 , TypeFamilies
 , TypeSynonymInstances
 , FlexibleInstances
 #-}

module Language where

import Prelude hiding(exp)

class Language f a where
  add :: f a (a,(a,()))
  eq  :: f a (a,(a,()))
  lit :: f a ()
