{-# LANGUAGE ScopedTypeVariables #-}

module Data.Metrics.Distance (
    EditCosts,
    cosineSim,
    damerauLevenshtein,
    defaultEditCosts,
    hamming,
    jaccardSim,
    jaro,
    jaroSim,
    jaroWinkler,
    jaroWinklerSim,
    jaroWinklerSimStd,
    jaroWinklerStd,
    levenshtein,
    optimalStringAlignment,
    sift1,
    sift1Std,
    ) where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Ratio
import Data.STRef
import qualified Data.Array as A
import qualified Data.Array.ST as SA
import qualified Data.Map as M
import qualified Data.Metrics.NGram as N
import qualified Data.Set as S
import qualified Data.Vector as V

data EditCosts a = EditCosts { insertCost     :: a
                             , deleteCost     :: a
                             , substituteCost :: a
                             , transposeCost  :: a
                             } deriving Show

defaultEditCosts :: EditCosts Int
defaultEditCosts = EditCosts 1 1 1 1

hamming :: (Eq a, Num b) => EditCosts b -> V.Vector a -> V.Vector a -> Maybe b
hamming EditCosts { substituteCost = scost } s1 s2
    | l1 == l2 = Just $ (*scost) $ fromIntegral $ length $
        filter notEqualAtIndex [0..l1 - 1]
    | otherwise = Nothing
    where l1 = V.length s1; l2 = V.length s2
          notEqualAtIndex i = s1 V.! i /= s2 V.! i

levenshtein :: (Eq a, Ord b, Num b) => EditCosts b -> V.Vector a -> V.Vector a -> b
levenshtein EditCosts { insertCost = icost, deleteCost = dcost,
                        substituteCost = scost } s1 s2 = arr A.! (l1, l2)
    where l1 = V.length s1; l2 = V.length s2
          subCost i j = if s1 V.! (i - 1) /= s2 V.! (j - 1) then scost else 0
          cost i j = minimum [ (arr A.! (i - 1, j))     + icost
                             , (arr A.! (i, j - 1))     + dcost
                             , (arr A.! (i - 1, j - 1)) + subCost i j]
          arr = A.array ((0, 0), (l1, l2)) $
                       [((0, i), fromIntegral i) | i <- [0..l1]]
                    ++ [((i, 0), fromIntegral i) | i <- [0..l2]]
                    ++ [((i, j), cost i j) | i <- [1..l1], j <- [1..l2]]

optimalStringAlignment :: (Eq a, Ord b, Num b) => EditCosts b -> V.Vector a -> V.Vector a -> b
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
                       [((0, i), fromIntegral i) | i <- [0..l1]]
                    ++ [((i, 0), fromIntegral i) | i <- [0..l2]]
                    ++ [((i, j), cost i j) | i <- [1..l1], j <- [1..l2]]

damerauLevenshtein :: forall a b. (Ord a, Ord b, Num b) => EditCosts b -> V.Vector a -> V.Vector a -> b
damerauLevenshtein EditCosts { insertCost = icost, deleteCost = dcost,
            substituteCost = scost, transposeCost = tcost } s1 s2 = runST $ do
    let l1 = V.length s1; l2 = V.length s2
        maxDist = fromIntegral $ l1 + l2
        chars = nub $ V.toList s1 ++ V.toList s2

    alphabet <- newSTRef $ M.fromList $ zip chars (repeat 0)

    let matBounds = ((-1, -1), (l1, l2))
    d <- SA.newArray matBounds 0 :: ST s (SA.STArray s (Int, Int) b)

    SA.writeArray d (-1, -1) maxDist
    forM_ [0..l1] $ \i -> do SA.writeArray d (i, 0) $ fromIntegral i
                             SA.writeArray d (i, -1) maxDist
    forM_ [0..l2] $ \i -> do SA.writeArray d (0, i) $ fromIntegral i
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
            substitution <- (+cost * scost) <$> SA.readArray d (i - 1, j - 1)
            v <- SA.readArray d (k - 1, l - 1)
            let transposition = tcost + v + fromIntegral (i - k - 2 + j - l)

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
          f (x : xs, v1, v2) = let v1' = sift x v1 v2
                                   v2' = sift (-x) v2 v1
                               in (xs, v1', v2')
          alternating = concat [[d, -d] | d <- [0..n `div` 2]]
          alternating' = if even n
                             then alternating
                             else alternating ++ [n `div` 2 + 1]

sift1Std :: Eq a => V.Vector a -> V.Vector a -> Int
sift1Std = sift1 10

sift2 :: (Eq a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
sift2 = undefined

sift2Std :: (Eq a, Floating b) => V.Vector a -> V.Vector a -> b
sift2Std = sift2 5

jaro :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaro s1 s2 = 1 - jaroSim s1 s2

-- Simplification of JWD by algebra
--
--   JWD l p a b
-- = 1 - JWS l p a b
-- = (1 - JS a b) * (1 - l * p)
-- = (JD a b) * (1 - l * p)
jaroWinkler :: Eq a => Int -> Ratio Int -> V.Vector a -> V.Vector a -> Ratio Int
jaroWinkler prefixLen p s1 s2 = jaro s1 s2 * (1 - fromIntegral prefixLen * p)

jaroWinklerStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerStd s1 s2 = 1 - jaroWinklerSimStd s1 s2

qgram :: Ord a => Int -> V.Vector a -> V.Vector a -> Int
qgram n = ((sum . uncurry (zipWith ((abs .) . (-))) . unzip) .) . N.qgram n

cosine :: (Ord a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
cosine n s1 s2 = 1 - cosineSim n s1 s2

jaccard :: Ord a => Int -> V.Vector a -> V.Vector a -> Ratio Int
jaccard n s1 s2 = 1 - jaccardSim n s1 s2

-----------------------------------------------------------------------------

matches :: Eq a => V.Vector a -> V.Vector a -> V.Vector Int
matches s1 s2 = V.imapMaybe f s1
    where l1 = V.length s1; l2 = V.length s2
          matchDist = (min l1 l2 `quot` 2) - 1
          f i e = let offset = max 0 (i + 1 - (matchDist + 1))
                      elemRange = V.drop offset $ V.take (i + 1 + matchDist) s2
                  in (+ offset) <$> V.elemIndex e elemRange

transpositions :: V.Vector Int -> Int
transpositions = V.length . V.filter id . (V.zipWith (>) <*> V.tail)

jaroSim :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroSim s1 s2 =
    let l1 = V.length s1; l2 = V.length s2
        [m, t] = map ($ matches s1 s2) [length, transpositions]
        sim = (m % l1 + m % l2 + (m - t) % m) / 3
    in if m == 0 then 0 else sim

-- p should not exceed 0.25
-- standard value for p is 0.1
jaroWinklerSim :: Eq a => Int -> Ratio Int -> V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerSim prefixLen p s1 s2 =
    let fixedLen = max 0 $ min 4 prefixLen
        prefix1 = V.take fixedLen s1; prefix2 = V.take fixedLen s2
        sharedLen = V.length $ V.filter id $ V.zipWith (==) prefix1 prefix2
        sim = jaroSim s1 s2
    in sim + fromIntegral sharedLen * p * (1 - sim)

jaroWinklerSimStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerSimStd = jaroWinklerSim 4 (1 % 10)

cosineSimilarity :: Floating a => [Int] -> [Int] -> a
cosineSimilarity x y =
    let sumProduct = fromIntegral . sum . uncurry (zipWith (*))
        [x2, y2, xy] = map sumProduct [(x, x), (y, y), (x, y)]
    in xy / (sqrt x2 * sqrt y2)

cosineSim :: (Ord a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
cosineSim n s1 s2 = uncurry cosineSimilarity $ unzip $ N.qgram n s1 s2

jaccardSim :: Ord a => Int -> V.Vector a -> V.Vector a -> Ratio Int
jaccardSim n s1 s2 =
    let n1 = N.ngram n s1; n2 = N.ngram n s2
        i = S.intersection n1 n2; u = S.union n1 n2
    in S.size i % S.size u

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

sift2Sim :: (Eq a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
sift2Sim maxOffset s1 s2 =
    let d = sift2 maxOffset s1 s2
        maxLen = max (V.length s1) (V.length s2)
    in if maxLen == 0 then 1 else 1 - d / fromIntegral maxLen

sift2StdSim :: (Eq a, Floating b) => V.Vector a -> V.Vector a -> b
sift2StdSim = sift2Sim 5
