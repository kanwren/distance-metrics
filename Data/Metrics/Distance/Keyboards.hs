module Data.Metrics.Distance.Keyboards (
    EditCosts,
    damerauLevenshtein,
    defaultEditCosts,
    dvorakEditCosts,
    euclideanCost,
    hamming,
    levenshtein,
    optimalStringAlignment,
    qwertyEditCosts,
    ) where

import qualified Data.Map.Strict as M
import Data.Metrics.Distance.Keyboards.Qwerty (qwertyCoords)
import Data.Metrics.Distance.Keyboards.Dvorak (dvorakCoords)

-- EditCosts are dynamic, calculated using some "quantum" type (usually Char),
-- and some numeric type for the cost
data EditCosts q a = EditCosts { insertCost :: q -> a
                               , deleteCost :: q -> a
                               , substituteCost :: q -> q -> a
                               , transposeCost :: q -> q -> a
                               } deriving Show

defaultEditCosts :: EditCosts q Int
defaultEditCosts = EditCosts { insertCost _ = 1
                             , deleteCost _ = 1
                             , substituteCost _ _ = 1
                             , transposeCost _ _ = 1
                             }

distance :: (Int, Int) -> (Int, Int) -> Double
distance (a, b) (c, d) = sqrt ((c' - a') ^ 2 + (d' - b') ^ 2)
    where [a', b', c', d'] = map fromIntegral [a, b, c, d]

euclideanCost :: M.Map Char (Int, Int) -> EditCosts Char Double
euclideanCost keys = EditCosts {
    insertCost c = maybe 1 (\d -> (/ maxDist) . distance (0, 0)) $
        M.lookup c keys,
    deleteCost c = maybe 1 (\d -> (/ maxDist) . distance (0, 0)) $
        M.lookup c keys,
    substituteCost c1 c2 = maybe 1 $ (\a b -> abs (a - b) / (maxDist * 2))
            <$> M.lookup c1 keys <*> M.lookup c2 keys,
    transposeCost c1 c2 = maybe 1 $ (\a b -> abs (a - b) / (maxDist * 2))
            <$> M.lookup c1 keys <*> M.lookup c2 keys
    }
    where maxDist = maximum $ map (distance (0, 0)) dists
          dists = map snd $ M.assocs keys

qwertyEditCosts :: EditCosts Char Double
qwertyEditCosts = euclideanCost qwertyCoords

dvorakEditCosts :: EditCosts Char Double
dvorakEditCosts = euclideanCost dvorakCoords
