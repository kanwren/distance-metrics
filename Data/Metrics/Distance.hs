{-# LANGUAGE ScopedTypeVariables #-}

module Data.Metrics.Distance (
    EditCosts,
    defaultEditCosts,
    damerauLevenshtein,
    hamming,
    levenshtein,
    optimalStringAlignment,
    ) where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef
import qualified Data.Array as A
import qualified Data.Array.ST as SA
import qualified Data.Map as M
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


