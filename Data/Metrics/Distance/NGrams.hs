module Data.Metrics.Distance.NGrams (
    cosine,
    cosineSim,
    jaccard,
    jaccardSim,
    jaro,
    jaroSim,
    jaroWinkler,
    jaroWinklerSim,
    jaroWinklerSimStd,
    jaroWinklerStd,
    qgram
    ) where

import Data.Ratio
import qualified Data.Set as S
import qualified Data.Vector as V

ngrams :: Ord a => Int -> V.Vector a -> S.Set (V.Vector a)
ngrams n v
    | n <= 0 = S.empty
    | n == 1 = S.fromList $ map V.singleton $ V.toList v
    | otherwise = S.fromList $ map (\i -> V.slice i 2 v) [0..V.length v - n]

qgrams :: Ord a => Int -> V.Vector a -> V.Vector a -> [(Int, Int)]
qgrams n v1 v2 =
    let n1 = ngrams n v1; n2 = ngrams n v2
        allNGrams = S.toList $ S.union n1 n2
        makeGram g = (fromEnum $ S.member g n1, fromEnum $ S.member g n2)
    in map makeGram allNGrams

-------------------------------------------------------------------------------

jaro :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaro s1 s2 = 1 - jaroSim s1 s2

-- Simplification of JWD by algebra
--
--   JWD l p a b
-- = 1 - JWS l p a b
-- = (1 - JS a b) * (1 - l * p)
-- = (JD a b) * (1 - l * p)
jaroWinkler
    :: Eq a
    => Int
    -> Ratio Int
    -> V.Vector a
    -> V.Vector a
    -> Ratio Int
jaroWinkler prefixLen p s1 s2 = jaro s1 s2 * (1 - fromIntegral prefixLen * p)

jaroWinklerStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
jaroWinklerStd s1 s2 = 1 - jaroWinklerSimStd s1 s2

qgram :: Ord a => Int -> V.Vector a -> V.Vector a -> Int
qgram n = ((sum . uncurry (zipWith ((abs .) . (-))) . unzip) .) . qgrams n

cosine :: (Ord a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
cosine n s1 s2 = 1 - cosineSim n s1 s2

jaccard :: Ord a => Int -> V.Vector a -> V.Vector a -> Ratio Int
jaccard n s1 s2 = 1 - jaccardSim n s1 s2

-------------------------------------------------------------------------------

matches :: Eq a => V.Vector a -> V.Vector a -> V.Vector Int
matches s1 s2 = V.imapMaybe f s1
    where l1 = V.length s1; l2 = V.length s2
          matchDist = (min l1 l2 `quot` 2) - 1
          f i e = let offset = max 0 (i + 1 - (matchDist + 1))
                      elemRange = V.drop offset $ V.take (i + 1 + matchDist) s2
                  in (+ offset) <$> V.elemIndex e elemRange

transpositions :: V.Vector Int -> Int
transpositions = V.length . V.filter id . (V.zipWith (>) <*> V.tail)

jaroSim
    :: Eq a
    => V.Vector a
    -> V.Vector a
    -> Ratio Int
jaroSim s1 s2 =
    let l1 = V.length s1; l2 = V.length s2
        [m, t] = map ($ matches s1 s2) [length, transpositions]
        sim = (m % l1 + m % l2 + (m - t) % m) / 3
    in if m == 0 then 0 else sim

-- p should not exceed 0.25
-- standard value for p is 0.1
jaroWinklerSim
    :: Eq a
    => Int
    -> Ratio Int
    -> V.Vector a
    -> V.Vector a
    -> Ratio Int
jaroWinklerSim prefixLen p s1 s2 =
    let fixedLen = max 0 $ min 4 prefixLen
        prefix1 = V.take fixedLen s1; prefix2 = V.take fixedLen s2
        sharedLen = V.length $ V.filter id $ V.zipWith (==) prefix1 prefix2
        sim = jaroSim s1 s2
    in sim + fromIntegral sharedLen * p * (1 - sim)

jaroWinklerSimStd
    :: Eq a
    => V.Vector a
    -> V.Vector a
    -> Ratio Int
jaroWinklerSimStd = jaroWinklerSim 4 (1 % 10)

cosineSimilarity
    :: Floating a
    => [Int]
    -> [Int]
    -> a
cosineSimilarity x y =
    let sumProduct = fromIntegral . sum . uncurry (zipWith (*))
        [x2, y2, xy] = map sumProduct [(x, x), (y, y), (x, y)]
    in xy / (sqrt x2 * sqrt y2)

cosineSim
    :: (Ord a, Floating b)
    => Int
    -> V.Vector a
    -> V.Vector a
    -> b
cosineSim n s1 s2 = uncurry cosineSimilarity $ unzip $ qgrams n s1 s2

jaccardSim
    :: Ord a
    => Int
    -> V.Vector a
    -> V.Vector a
    -> Ratio Int
jaccardSim n s1 s2 =
    let n1 = ngrams n s1; n2 = ngrams n s2
        i = S.intersection n1 n2; u = S.union n1 n2
    in S.size i % S.size u
