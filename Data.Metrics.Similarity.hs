module Data.Metrics.Similarity (
    jaro,
    jaroWinkler,
    jaroWinklerStd
    ) where

import Data.Ratio
import qualified Data.Metrics.NGram as N

jaro :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaro s1 s2 =
    let l1 = V.length s1; l2 = V.length s2
        [m, t] = map ($ matches s1 s2) [length, transpositions]
        sim = (m % l1 + m % l2 + (m - t) % m) / 3
    in if m == 0 then fromIntegral 0 else sim

-- p should not exceed 0.25
-- standard value for p is 0.1
jaroWinkler :: Eq a => V.Vector a -> V.Vector a -> Int -> Ratio Int -> Ratio Int
jaroWinkler s1 s2 prefixLen p =
    let fixedLen = max 0 $ min 4 prefixLen
        prefix1 = V.take fixedLen s1; prefix2 = V.take fixedLen s2
        sharedLen = V.length $ V.filter id $ V.zipWith (==) prefix1 prefix2
        sim = jaro s1 s2
    in sim + fromIntegral sharedLen * p * (1 - sim)

jaroWinklerStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerStd s1 s2 = jaroWinkler s1 s2 4 (1 % 10)

cosine :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
cosine = undefined

jaccard :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaccard = undefined
