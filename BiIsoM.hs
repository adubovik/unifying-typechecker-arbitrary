{-# language
   NamedFieldPuns
 , ScopedTypeVariables
 #-}

module BiIsoM where

import Control.Monad

import DataConstructorM

data BiIsoM m a as bs b =
  BiIsoM { forward  :: a -> m (as, bs -> m b)
         , backward :: b -> m (bs, as -> m a)
         }

runForward :: Monad m => BiIsoM m i () () o -> (i -> m o)
runForward BiIsoM{forward} =
  \i -> do
    ((),mo) <- forward i
    mo ()

runBackward :: Monad m => BiIsoM m a () () b -> (b -> m a)
runBackward BiIsoM{backward} =
  \o -> do
    ((),mi) <- backward o
    mi ()

mkBiIsoM :: Monad m =>
            DCM m a as -> DCM m b bs ->
            BiIsoM m a as bs b
mkBiIsoM adc bdc = BiIsoM
  { forward = \a -> do
       as <- unapply adc a
       return (as, apply bdc)
  , backward = \b -> do
       bs <- unapply bdc b
       return (bs, apply adc)
  }

(<*>) :: Monad m =>
         BiIsoM m a (x,xs) (y,ys) b ->
         BiIsoM m x (    ) (    ) y ->
         BiIsoM m a    xs     ys  b
(<*>) BiIsoM{ forward  = fM :: a -> m ((x,xs), (y,ys) -> m b)
            , backward = bM :: b -> m ((y,ys), (x,xs) -> m a)
            }
      BiIsoM{ forward  = fm :: x -> m ((), () -> m y)
            , backward = bm :: y -> m ((), () -> m x)
            } =
  BiIsoM
  { forward  = (\a -> do
      ((x,xs), fy) <- fM a
      ((),my) <- fm x
      y <- my ()
      return (xs, (\ys -> fy (y,ys)))
    ) :: a -> m (xs, ys -> m b)

  , backward = (\b -> do
      ((y,ys), fx) <- bM b
      ((),mx) <- bm y
      x <- mx ()
      return (ys, (\xs -> fx (x,xs)))
    ) :: b -> m (ys, xs -> m a)

  }

(<|>) :: MonadPlus m =>
         BiIsoM m i is os o ->
         BiIsoM m i is os o ->
         BiIsoM m i is os o
(<|>)  BiIsoM{forward = fx, backward = bx}
      ~BiIsoM{forward = fy, backward = by} =
  BiIsoM
  { forward  = \i -> fx i `mplus` fy i
  , backward = \o -> bx o `mplus` by o
  }
