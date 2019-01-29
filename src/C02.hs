{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module C02 where

import           Data.Maybe                     ( fromMaybe )

-- 2.1 - Write a function suffixes that takes a list xs and returns a list of
-- all the suffixes of xs in decreasing order of length. For example:
-- suffixes [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4],[]]

suffixes :: [a] -> [[a]]
suffixes []         = []
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

-- 2.2 - Rewrite member to take no more than d + 1 comparisons by keeping track
-- of a candidate element that might be equal to the query element and checking
-- for equality only when you hit the bottom of the tree.

member22 :: Ord a => a -> UnbalancedSet a -> Bool
member22 = go Nothing
 where
  go Nothing  _ E = False
  go (Just c) x E = c == x
  go c x (T a y b) | x <= y    = go (Just y) x a
                   | otherwise = go c x b

-- 2.3 - Inserting an existing element into a binary search tree copies the
-- entire search path even though the copied nodes are indistinguishable from
-- the originals. Rewrite insert using exceptions to avoid this copying.

insert23 :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
insert23 x s = fromMaybe s $ go s
 where
  go E = Just $ T E x E
  go (T a y b) | x < y     = T <$> go a <*> pure y <*> pure b
               | x > y     = T <$> pure a <*> pure y <*> go b
               | otherwise = Nothing

main :: IO ()
main = do
  putStrLn "2.1:"
  print $ suffixes @Int [1, 2, 3, 4]

  let s = insert "sit" $ insert "dolor" $ insert "ipsum" $ insert "lorem" E
  putStrLn "\nSet/UnbalancedSet:"
  print s
  print $ member "dolor" s
  print $ member "should_not_exist" s

  putStrLn "\n2.2:"
  print $ member22 "dolor" s
  print $ member22 "should_not_exist" s

  putStrLn "\n2.3:"
  let s23 =
        insert23 "dolor"
          $ insert23 "sit"
          $ insert23 "dolor"
          $ insert23 "ipsum"
          $ insert23 "lorem" E
  print s23
