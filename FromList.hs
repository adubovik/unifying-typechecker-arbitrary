module FromList where

import ListLogic

class FromList f where
  fromList :: [a] -> f a

instance FromList [] where
  fromList = id

instance FromList LL where
  fromList = LL
