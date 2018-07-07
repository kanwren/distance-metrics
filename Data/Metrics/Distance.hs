module Data.Metrics.Distance (
    EditCosts,
    damerauLevenshtein,
    defaultEditCosts,
    hamming,
    jaro,
    jaroWinkler,
    jaroWinklerStd,
    levenshtein,
    optimalStringAlignment,
    sift1,
    sift1Std
    ) where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Ratio
import Data.STRef
import qualified Data.Array as A
import qualified Data.Array.ST as SA
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Metrics.Similarity as SM
import qualified Data.Metrics.NGram as N

data EditCosts = EditCosts { insertCost     :: Int
                           , deleteCost     :: Int
                           , substituteCost :: Int
                           , transposeCost  :: Int
                           }

defaultEditCosts :: EditCosts
defaultEditCosts = EditCosts 1 1 1 1

hamming :: Eq a => EditCosts -> V.Vector a -> V.Vector a -> Maybe Int
hamming EditCosts { substituteCost = scost } s1 s2
    | l1 == l2 = Just $ (*scost) $ length $ filter notEqualAtIndex [0..l1 - 1]
    | otherwise = Nothing
    where l1 = V.length s1; l2 = V.length s2
          notEqualAtIndex i = s1 V.! i /= s2 V.! i

levenshtein :: Eq a => EditCosts -> V.Vector a -> V.Vector a -> Int
levenshtein EditCosts { insertCost = icost, deleteCost = dcost,
                        substituteCost = scost } s1 s2 = arr A.! (l1, l2)
    where l1 = V.length s1; l2 = V.length s2
          subCost i j = if s1 V.! (i - 1) /= s2 V.! (j - 1) then scost else 0
          cost i j = minimum [ (arr A.! (i - 1, j))     + icost
                             , (arr A.! (i, j - 1))     + dcost
                             , (arr A.! (i - 1, j - 1)) + subCost i j]
          arr = A.array ((0, 0), (l1, l2)) $
                       [((0, i), i)        | i <- [0..l1]]
                    ++ [((i, 0), i)        | i <- [0..l2]]
                    ++ [((i, j), cost i j) | i <- [1..l1], j <- [1..l2]]

optimalStringAlignment :: Eq a => EditCosts -> V.Vector a -> V.Vector a -> Int
optimalStringAlignment EditCosts { insertCost = icost, deleteCost = dcost,
    substituteCost = scost, transposeCost = tcost } s1 s2 = arr A.! (l1, l2)
    where l1 = V.length s1; l2 = V.length s2
          cost i j = let elemEq = s1 V.! (i - 1) == s2 V.! (j - 1)
                         subCost = if elemEq then 0 else scost
                         canTrans = i > 1 && j > 1 && not elemEq
                                 && s1 V.! (i - 2) == s2 V.! (j - 1)
                                 && s1 V.! (i - 1) == s2 V.! (j - 2)
                         levCost = minimum [ (arr A.! (i - 1, j))     + icost
                                           , (arr A.! (i, j - 1))     + dcost
                                           , (arr A.! (i - 1, j - 1)) + subCost]
                         transCost = arr A.! (i - 2, j - 2) +
                                         if elemEq then 0 else tcost
                     in if canTrans then min levCost transCost else levCost
          arr = A.array ((0, 0), (l1, l2)) $
                       [((0, i), i)        | i <- [0..l1]              ]
                    ++ [((i, 0), i)        | i <- [0..l2]              ]
                    ++ [((i, j), cost i j) | i <- [1..l1], j <- [1..l2]]

damerauLevenshtein :: Ord a => EditCosts -> V.Vector a -> V.Vector a -> Int
damerauLevenshtein EditCosts { insertCost = icost, deleteCost = dcost,
            substituteCost = scost, transposeCost = tcost } s1 s2 = runST $ do
    let l1 = V.length s1; l2 = V.length s2
        maxDist = l1 + l2
        chars = nub $ V.toList s1 ++ V.toList s2

    alphabet <- newSTRef $ M.fromList $ zip chars (repeat 0)

    let matBounds = ((-1, -1), (l1, l2))
    d <- SA.newArray matBounds 0 :: ST s (SA.STUArray s (Int, Int) Int)

    SA.writeArray d (-1, -1) maxDist
    forM_ [0..l1] $ \i -> do SA.writeArray d (i, 0) i
                             SA.writeArray d (i, -1) maxDist
    forM_ [0..l2] $ \i -> do SA.writeArray d (0, i) i
                             SA.writeArray d (-1, i) maxDist

    db <- newSTRef 0
    forM_ [1..l1] $ \i -> do
        let c1 = s1 V.! (i - 1)
        writeSTRef db 0
        forM_ [1..l2] $ \j -> do
            currentAlphabet <- readSTRef alphabet
            l <- readSTRef db
            let c2 = s2 V.! (j - 1)
                charsEqual = c1 == c2
                cost = if charsEqual then 0 else 1
                k = currentAlphabet M.! c2
            when charsEqual $ writeSTRef db j

            insertion <- (+icost) <$> SA.readArray d (i, j - 1)
            deletion <- (+dcost) <$> SA.readArray d (i - 1, j)
            substitution <- (+cost * scost) <$>
                                    SA.readArray d (i - 1, j - 1)
            v <- SA.readArray d (k - 1, l - 1)
            let transposition = v + (i - k - 1) + (j - l - 1) + tcost

            SA.writeArray d (i, j) $ minimum [insertion, deletion,
                                             substitution, transposition]

        modifySTRef alphabet $ M.insert c1 i

    SA.readArray d (l1, l2)

-- Offset is how much the first string will be shifted by
sift :: Eq a => Int -> V.Vector a -> V.Vector a -> V.Vector a
sift offset s1 s2 = V.imapMaybe f s1
    where f i x = let i' = i + offset
                  in if i' < 0 || i' >= V.length s2 || s1 V.! i /= s2 V.! i'
                         then Just x
                         else Nothing

sift1 :: Eq a => Int -> V.Vector a -> V.Vector a -> Int
sift1 n s1 s2 = max (V.length s1') (V.length s2') + n
    where fst3 x = let (a, _, _) = x in a
          (_, s1', s2') = until (null . fst3) f (alternating', s1, s2)
          f ([], _, _) = error "Error in sift1: until did not match"
          f ((x:xs), v1, v2) = let v1' = sift x v1 v2
                                   v2' = sift (-x) v2 v1
                               in (xs, v1', v2')
          alternating = concat $ [[d, -d] | d <- [0..n `div` 2]]
          alternating' = if even n
                             then alternating
                             else alternating ++ [n `div` 2 + 1]

sift1Std :: Eq a => V.Vector a -> V.Vector a -> Int
sift1Std = sift1 10

jaro :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaro s1 s2 = 1 - SM.jaro s1 s2

-- Simplification of JWD by algebra
--
--   JWD l p a b
-- = 1 - JWS l p a b
-- = (1 - JS a b) * (1 - l * p)
-- = (JD a b) * (1 - l * p)
jaroWinkler :: Eq a => Int -> Ratio Int -> V.Vector a -> V.Vector a -> Ratio Int
jaroWinkler prefixLen p s1 s2 = (jaro s1 s2) * (1 - fromIntegral prefixLen * p)

jaroWinklerStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerStd s1 s2 = 1 - SM.jaroWinklerStd s1 s2

longestCommonSubstring :: Eq a => V.Vector a -> V.Vector a -> Int
longestCommonSubstring = undefined

qgram :: Ord a => Int -> V.Vector a -> V.Vector a -> Int
qgram n = ((sum . uncurry (zipWith ((abs .) . (-))) . unzip) .) . N.qgram n

cosine :: (Ord a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
cosine n s1 s2 = 1 - SM.cosine n s1 s2

jaccard :: Ord a => Int -> V.Vector a -> V.Vector a -> Ratio Int
jaccard n s1 s2 = 1 - SM.jaccard n s1 s2

