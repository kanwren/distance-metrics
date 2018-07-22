module Data.Metrics.Distance.Sift (
    sift1,
    sift1Std,
    sift2,
    sift2Sim,
    sift2SimStd,
    sift2Std,
    sift3,
    sift3Sim,
    sift3SimStd,
    sift3Std,
    ) where

import Data.List
import Data.Ratio
import qualified Data.Vector as V

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

elemOffset :: Eq a => a -> Int -> Int -> V.Vector a -> Maybe Int
elemOffset e start maxOffset v =
    V.elemIndex e $ V.take maxOffset $ V.drop start v

sift2 :: Eq a => Int -> V.Vector a -> V.Vector a -> Ratio Int
sift2 offset s1 s2
    | V.null s1 = fromIntegral l2
    | V.null s2 = fromIntegral l1
    | otherwise = go 0 0 0 0
    where l1 = V.length s1; l2 = V.length s2
          go d c o1 o2
            | c + o1 >= l1 || c + o2 >= l2 =
                fromIntegral (d - c) + (l1 - o1 + l2 - o2) % 2
            | s1 V.! (c + o1) == s2 V.! (c + o2) = go d (c + 1) o1 o2
            | otherwise =
                case elemOffset (s2 V.! c) c offset s1 of
                    Just n -> go (d + n) (c + 1) n 0
                    Nothing ->
                        case elemOffset (s1 V.! c) c offset s2 of
                            Just n -> go (d + n) (c + 1) 0 n
                            Nothing -> go (d + 1) (c + 1) 0 0

{-
sift2' :: Eq a => Int -> [a] -> [a] -> Ratio Int
sift2' _ [] [] = 0
sift2' _ [] s2 = fromIntegral $ length s2
sift2' _ s1 [] = fromIntegral $ length s1
sift2' offset s1 s2 = go 0 s1 s2
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
-}

sift2Sim :: Eq a => Int -> V.Vector a -> V.Vector a -> Ratio Int
sift2Sim maxOffset s1 s2 =
    let d = sift2 maxOffset s1 s2
        maxLen = max (V.length s1) (V.length s2)
    in if maxLen == 0 then 1 else 1 - d / fromIntegral maxLen

sift2Std :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
sift2Std = sift2 5

sift2SimStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
sift2SimStd = sift2Sim 5

--------------------------------------------------------------------------------

sift3 :: Eq a => Int -> V.Vector a -> V.Vector a -> Ratio Int
sift3 offset s1 s2
    | V.null s1 = fromIntegral l2
    | V.null s2 = fromIntegral l1
    | otherwise = (l1 + l2) % 2 - fromIntegral lcs
    where l1 = V.length s1; l2 = V.length s2
          lcs = go 0 0 0 0
          go :: Int -> Int -> Int -> Int -> Int
          go c l o1 o2
            | c + o1 >= l1 || c + o2 >= l2 = l
            | s1 V.! (c + o1) == s2 V.! (c + o2) = go (c + 1) (l + 1) o1 o2
            | otherwise = uncurry (go (c + 1) l) $
                case elemOffset (s2 V.! c) c offset s1 of
                    Just n -> (n, 0)
                    Nothing -> case elemOffset (s1 V.! c) c offset s2 of
                                   Just n -> (0, n)
                                   Nothing -> (0, 0)

sift3Sim :: Eq a => Int -> V.Vector a -> V.Vector a -> Ratio Int
sift3Sim maxOffset s1 s2 =
    let d = sift3 maxOffset s1 s2
        maxLen = max (V.length s1) (V.length s2)
    in if maxLen == 0 then 1 else 1 - d / fromIntegral maxLen

sift3Std :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
sift3Std = sift3 5

sift3SimStd :: Eq a => V.Vector a -> V.Vector a -> Ratio Int
sift3SimStd = sift3Sim 5
