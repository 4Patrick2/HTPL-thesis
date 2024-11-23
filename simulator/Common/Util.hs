module Common.Util
    ( module Common.Util
    ) where

import Data.List ( nub, (\\) )

-----------------------------
-- | Common utility functions
-----------------------------

extractDuplicates :: Eq a => [a] -> [a]
extractDuplicates ls = nub $ ls \\ nub ls

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs