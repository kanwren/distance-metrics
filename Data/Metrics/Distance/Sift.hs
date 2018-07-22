module Data.Metrics.Distance.Sift (
    sift1,
    sift2,
    sift2Sim,
    ) where

import Data.List
import Data.Ratio
import qualified Data.Vector as V

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

sift2 :: Eq a => Int -> V.Vector a -> V.Vector a -> Ratio Int
sift2 offset s1 s2
    | V.null s1 = l2 % 2
    | V.null s2 = l1 % 2
    | otherwise = go 0 0 0 0
    where l1 = V.length s1; l2 = V.length s2
          elemOffset e start v = V.elemIndex e $ V.take offset $ V.drop start v
          go d c o1 o2
            | c + o1 >= l1 || c + o2 >= l2 =
                fromIntegral (d - c) + (l1 - o1 + l2 - o2) % 2
            | otherwise =
                case elemOffset (s2 V.! c) c s1 of
                    Just n -> go (d + n) (c + 1) (o1 + n) o2
                    Nothing ->
                        case elemOffset (s1 V.! c) c s2 of
                            Just n -> go (d + n) (c + 1) o1 (o2 + n)
                            Nothing -> go (d + 1) (c + 1) o1 o2

sift2' :: Eq a => Int -> [a] -> [a] -> Ratio Int
sift2' offset = go 0
    where go d [] s2 = fromIntegral d + length s2 % 2
          go d s1 [] = fromIntegral d + length s1 % 2
          go d (x:xs) (y:ys)
            | x == y = go d xs ys
            | otherwise =
                case elemIndex y (take offset xs) of
                    Just n -> go (d + n) (drop (n + 1) xs) ys
                    Nothing -> case elemIndex x (take offset ys) of
                        Just n -> go (d + n) (drop (n + 1) ys) xs
                        Nothing -> go (d + 1) xs ys

sift2Sim :: Eq a => Int -> V.Vector a -> V.Vector a -> Ratio Int
sift2Sim maxOffset s1 s2 =
    let d = sift2 maxOffset s1 s2
        maxLen = max (V.length s1) (V.length s2)
    in if maxLen == 0 then 1 else 1 - d / fromIntegral maxLen
