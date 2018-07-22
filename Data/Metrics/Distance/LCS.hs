module Data.Metrics.Distance.LCS (
    longestCommonSubstring,
    longestCommonSubsequence
    ) where

import qualified Data.Array as A
import qualified Data.Vector as V

longestCommonSubstring :: Eq a => V.Vector a -> V.Vector a -> Int
longestCommonSubstring s1 s2 = maximum $ A.elems arr
    where l1 = V.length s1; l2 = V.length s2
          compute i j = if s1 V.! (i - 1) == s2 V.! (j - 1)
                            then arr A.! (i - 1, j - 1) + 1
                            else 0
          arr = A.array ((0, 0), (l1, l2)) $
                        [((0, i), 0)           | i <- [0..l1]              ]
                     ++ [((i, 0), 0)           | i <- [0..l2]              ]
                     ++ [((i, j), compute i j) | i <- [1..l1], j <- [1..l2]]

longestCommonSubsequence :: Eq a => V.Vector a -> V.Vector a -> Int
longestCommonSubsequence s1 s2 = arr A.! (l1, l2)
    where l1 = V.length s1; l2 = V.length s2
          compute i j = if s1 V.! (i - 1) == s2 V.! (j - 1)
                            then arr A.! (i - 1, j - 1) + 1
                            else max (arr A.! (i - 1, j)) (arr A.! (i, j - 1))
          arr = A.array ((0, 0), (l1, l2)) $
                        [((0, i), i)           | i <- [0..l1]              ]
                     ++ [((i, 0), i)           | i <- [0..l2]              ]
                     ++ [((i, j), compute i j) | i <- [1..l1], j <- [1..l2]]
