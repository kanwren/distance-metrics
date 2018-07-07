module Data.Metrics.Distance (
    EditCosts,
    defaultEditCosts,
    hamming,
    levenshtein,
    optimalStringAlignment,
    damerauLevenshtein,
    sift1,
    jaro,
    jaroWinkler,
    jaroWinklerStd,
    onStr,
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

data EditCosts = EditCosts { insertCost     :: Int
                           , deleteCost     :: Int
                           , substituteCost :: Int
                           , transposeCost  :: Int
                           }

defaultEditCosts :: EditCosts
defaultEditCosts = EditCosts 1 1 1 1

hamming :: Eq a => V.Vector a -> V.Vector a -> EditCosts -> Maybe Int
hamming s1 s2 EditCosts { substituteCost = scost }
    | l1 == l2 = Just $ (*scost) $ length $ filter notEqualAtIndex [0..l1 - 1]
    | otherwise = Nothing
    where l1 = V.length s1; l2 = V.length s2
          notEqualAtIndex i = s1 V.! i /= s2 V.! i

levenshtein :: Eq a => V.Vector a -> V.Vector a -> EditCosts -> Int
levenshtein s1 s2 EditCosts { insertCost = icost, deleteCost = dcost,
                              substituteCost = scost } = arr A.! (l1, l2)
    where l1 = V.length s1; l2 = V.length s2
          subCost i j = if s1 V.! (i - 1) /= s2 V.! (j - 1) then scost else 0
          cost i j = minimum [ (arr A.! (i - 1, j))     + icost
                             , (arr A.! (i, j - 1))     + dcost
                             , (arr A.! (i - 1, j - 1)) + subCost i j]
          arr = A.array ((0, 0), (l1, l2)) $
                       [((0, i), i)        | i <- [0..l1]]
                    ++ [((i, 0), i)        | i <- [0..l2]]
                    ++ [((i, j), cost i j) | i <- [1..l1], j <- [1..l2]]

optimalStringAlignment :: Eq a => V.Vector a -> V.Vector a -> EditCosts -> Int
optimalStringAlignment s1 s2 EditCosts { insertCost = icost, deleteCost = dcost,
            substituteCost = scost, transposeCost = tcost } = arr A.! (l1, l2)
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

damerauLevenshtein :: Ord a => V.Vector a -> V.Vector a -> EditCosts -> Int
damerauLevenshtein s1 s2 EditCosts { insertCost = icost, deleteCost = dcost,
            substituteCost = scost, transposeCost = tcost } = runST $ do
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

sift1 :: Eq a => V.Vector a -> V.Vector a -> Int -> Int
sift1 = undefined

matches :: Eq a => V.Vector a -> V.Vector a -> V.Vector Int
matches s1 s2 = V.imapMaybe f s1
    where l1 = V.length s1; l2 = V.length s2
          matchDist = (min l1 l2 `quot` 2) - 1
          f i e = let offset = max 0 (i + 1 - (matchDist + 1))
                      elemRange = V.drop offset $ V.take (i + 1 + matchDist) s2
                  in (+ offset) <$> V.elemIndex e elemRange

transpositions :: V.Vector Int -> Int
transpositions = V.length . V.filter id . (V.zipWith (>) <*> V.tail)

jaro :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaro s1 s2 = SM.jaro s1 s2

jaroWinkler :: Eq a => V.Vector a -> V.Vector a -> Int -> Ratio Int -> Ratio Int
jaroWinkler s1 s2 prefixLen p = 1 - SM.jaroWinkler s1 s2 prefixLen p

jaroWinklerStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerStd s1 s2 = 1 - SM.jaroWinklerStd s1 s2

longestCommonSubstring :: Eq a => V.Vector a -> V.Vector a -> Int
longestCommonSubstring = undefined

qgram :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
qgram = undefined

cosine :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
cosine s1 s2 = 1 - SM.cosine s1 s2

jaccard :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaccard s1 s2 = 1 - SM.jaccard s1 s2

onStr :: (V.Vector a -> V.Vector a -> b) -> [a] -> [a] -> b
onStr distanceCalc = distanceCalc `on` V.fromList
    where on f g x y = f (g x) (g y)

