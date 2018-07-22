module Data.Metrics.Distance.Sift (
    sift1,
    sift1Std,
    sift2,
    sift2Std,
    sift2Sim,
    sift2SimStd,
    ) where

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

sift2 :: (Eq a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
sift2 = undefined

sift2Std :: (Eq a, Floating b) => V.Vector a -> V.Vector a -> b
sift2Std = sift2 5

sift2Sim :: (Eq a, Floating b) => Int -> V.Vector a -> V.Vector a -> b
sift2Sim maxOffset s1 s2 =
    let d = sift2 maxOffset s1 s2
        maxLen = max (V.length s1) (V.length s2)
    in if maxLen == 0 then 1 else 1 - d / fromIntegral maxLen

sift2SimStd :: (Eq a, Floating b) => V.Vector a -> V.Vector a -> b
sift2SimStd = sift2Sim 5
