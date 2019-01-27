{-# Language TypeApplications #-}

module C02 where

-- 2.1 - Write a function suffixes that takes a list xs and returns a list of
-- all the suffixes of xs in decreasing order of length. For example:
-- suffixes [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4],[]]

suffixes :: [a] -> [[a]]
suffixes []         = [[]]
suffixes xs@(_ : t) = xs : suffixes t

main :: IO ()
main = print $ suffixes @Int [1, 2, 3, 4]
