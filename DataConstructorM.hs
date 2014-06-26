module DataConstructorM where

-- DCM stands for Data Construcor.
-- It explicitly caputes pattern matching and
-- datatype construction.
data DCM m a b = DCM { unapply :: a -> m b
                     , apply   :: b -> m a
                     }
