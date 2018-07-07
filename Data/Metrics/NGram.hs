module Data.Metrics.NGram (
    ngram,
    qgram
    ) where

import qualified Data.Vector as V
import qualified Data.Set as S

ngram :: Ord a => Int -> V.Vector a -> S.Set (V.Vector a)
ngram n v = S.fromList $ map (\i -> V.slice i 2 v) [0..V.length v - n]

qgram :: Ord a => Int -> V.Vector a -> V.Vector a -> [(Int, Int)]
qgram n v1 v2 =
    let n1 = ngram n v1; n2 = ngram n v2
        allNGrams = S.toList $ S.union n1 n2
        makeGram g = (fromEnum $ S.member g n1, fromEnum $ S.member g n2)
    in map makeGram allNGrams
