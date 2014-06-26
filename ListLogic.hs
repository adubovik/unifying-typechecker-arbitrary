module ListLogic where

import Control.Monad.Logic

data LL a = LL { unLL :: [a] }
  deriving (Show,Eq)

instance Monad LL where
  return = LL . return
  LL a >>= f = LL $ a >>- (unLL . f)

instance MonadPlus LL where
  mzero = LL mzero
  LL x `mplus` LL y = LL $ x `interleave` y
