module Data.Metrics.Similarity (
    cosine,
    jaccard
    jaro,
    jaroWinkler,
    jaroWinklerStd,
    ) where

import Data.Ratio
import qualified Data.Metrics.NGram as N
import qualified Data.Set as S
import qualified Data.Vector as V

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
jaro s1 s2 =
    let l1 = V.length s1; l2 = V.length s2
        [m, t] = map ($ matches s1 s2) [length, transpositions]
        sim = (m % l1 + m % l2 + (m - t) % m) / 3
    in if m == 0 then 0 else sim

-- p should not exceed 0.25
-- standard value for p is 0.1
jaroWinkler :: Eq a => Int -> Ratio Int -> V.Vector a -> V.Vector a -> Ratio Int
jaroWinkler prefixLen p s1 s2 =
    let fixedLen = max 0 $ min 4 prefixLen
        prefix1 = V.take fixedLen s1; prefix2 = V.take fixedLen s2
        sharedLen = V.length $ V.filter id $ V.zipWith (==) prefix1 prefix2
        sim = jaro s1 s2
    in sim + fromIntegral sharedLen * p * (1 - sim)

jaroWinklerStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerStd = jaroWinkler 4 (1 % 10)

cosineSimilarity :: Floating a => [Int] -> [Int] -> a
cosineSimilarity x y =
    let sumProduct = fromIntegral . sum . uncurry (zipWith (*))
        [x2, y2, xy] = map sumProduct [(x, x), (y, y), (x, y)]
    in xy / (sqrt x2 * sqrt y2)

cosine :: (Ord a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
cosine n s1 s2 = uncurry cosineSimilarity $ unzip $ N.qgram n s1 s2

jaccard :: Ord a => Int -> V.Vector a -> V.Vector a -> Ratio Int
jaccard n s1 s2 =
    let n1 = N.ngram n s1; n2 = N.ngram n s2
        i = S.intersection n1 n2; u = S.union n1 n2
    in S.size i % S.size u

