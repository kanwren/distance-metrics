module Data.Metrics.Distance.Keyboards (
    EditCosts,
    damerauLevenshtein,
    defaultEditCosts,
    hamming,
    levenshtein,
    optimalStringAlignment,
    ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Metrics.Distance.Keyboards.Qwerty (qwertyCoords)
import Data.Metrics.Distance.Keyboards.Dvorak (dvorakCoords)

-- EditCosts are dynamic, calculated using some "quantum" type (usually Char),
-- and some numeric type for the cost
data EditCosts q a = EditCosts { insertCost :: q -> a
                               , deleteCost :: q -> a
                               , substituteCost :: q -> q -> a
                               , transposeCost :: q -> q -> a
                               }

defaultEditCosts :: EditCosts q Int
defaultEditCosts = EditCosts { insertCost = const 1
                             , deleteCost = const 1
                             , substituteCost = const (const 1)
                             , transposeCost = const (const 1)
                             }

distance :: (Int, Int) -> (Int, Int) -> Double
distance (a, b) (c, d) = sqrt ((c' - a') ^ 2 + (d' - b') ^ 2)
    where [a', b', c', d'] = map fromIntegral [a, b, c, d]

euclideanCost :: M.Map Char (Int, Int) -> EditCosts Char Double
euclideanCost keys = EditCosts {
    insertCost = \c -> maybe 1 ((/ maxDist) . distance (0, 0)) $
        M.lookup c keys,
    deleteCost = \c -> maybe 1 ((/ maxDist) . distance (0, 0)) $
        M.lookup c keys,
    substituteCost = \c1 c2 ->
        fromMaybe 1 ((\a b -> distance a b / (maxDist * 2))
            <$> M.lookup c1 keys <*> M.lookup c2 keys),
    transposeCost = \c1 c2 ->
        fromMaybe 1 ((\a b -> distance a b / (maxDist * 2))
            <$> M.lookup c1 keys <*> M.lookup c2 keys)
    }
    where maxDist = maximum $ map (distance (0, 0)) dists
          dists = map snd $ M.assocs keys

qwertyEditCosts :: EditCosts Char Double
qwertyEditCosts = euclideanCost qwertyCoords

dvorakEditCosts :: EditCosts Char Double
dvorakEditCosts = euclideanCost dvorakCoords


damerauLevenshtein :: (Ord c, Num a) => EditCosts c a -> V.Vector c -> V.Vector c -> a
damerauLevenshtein = undefined

hamming :: (Eq c, Num a) => EditCosts c a -> V.Vector c -> V.Vector c -> a
hamming = undefined

levenshtein :: (Eq c, Ord a, Num a) => EditCosts c a -> V.Vector c -> V.Vector c -> a
levenshtein = undefined

optimalStringAlignment :: (Eq c, Ord a, Num a) => EditCosts c a -> V.Vector c -> V.Vector c -> a
optimalStringAlignment = undefined
