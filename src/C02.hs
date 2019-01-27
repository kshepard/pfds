{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# Language TypeApplications #-}

module C02 where

-- 2.1 - Write a function suffixes that takes a list xs and returns a list of
-- all the suffixes of xs in decreasing order of length. For example:
-- suffixes [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4],[]]

suffixes :: [a] -> [[a]]
suffixes []         = [[]]
suffixes xs@(_ : t) = xs : suffixes t

-- Implementation of Set/UnbalancedSet

class Set s a where
  empty  :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)
  deriving (Show)

instance Ord a => Set UnbalancedSet a where
  empty = E

  member _ E = False
  member x (T a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x E = T E x E
  insert x s@(T a y b)
    | x < y     = T (insert x a) y b
    | x > y     = T a y (insert x b)
    | otherwise = s

main :: IO ()
main = do
  print $ suffixes @Int [1, 2, 3, 4]

  let s = insert "sit" $ insert "dolor" $ insert "ipsum" $ insert "lorem" E
  print s
  print $ member "dolor" s
  print $ member "should_not_exist" s
