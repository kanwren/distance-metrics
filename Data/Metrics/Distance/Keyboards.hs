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

import Data.Map as M

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

type KeyCoords = M.Map Char (Int, Int)

qwertyEditCosts :: EditCosts Char Double
qwertyEditCosts = euclideanCost qwertyCoords

dvorakEditCosts :: EditCosts Char Double
dvorakEditCosts = euclideanCost dvorakCoords

euclideanCost :: KeyCoords -> EditCosts Char Double
euclideanCost = undefined

qwertyCoords :: KeyCoords
qweryCoords = M.union qwertyUnshifted qwertyShifted

qwertyShifted :: M.Map Char Char
qwertyShifted = undefined

qwertyUnshifted :: KeyCoords
qwertyUnshifted = M.fromList $
    [ ('h', (1, 0)), ('j', (2, 0)), ('k', (3, 0))
    , ('l', (4, 0)), (';', (5, 0)), ('\'', (6, 0))
    , ('\n', (7, 0))

    , ('g', (-1, 0)), ('f', (-2, 0))
    , ('d', (-3, 0)), ('s', (-4, 0)), ('a', (-5, 0))

    , ('y', (1, 1)), ('u', (2, 1)), ('i', (3, 1))
    , ('o', (4, 1)), ('p', (5, 1)), ('[', (6, 1))
    , (']', (7, 1)), ('\\', (8, 1))

    , ('t', (-1, 1)), ('r', (-2, 1)), ('e', (-3, 1))
    , ('w', (-4, 1)), ('q', (-5, 1)), ('\t', (-6, 1))

    , ('n', (1, -1)), ('m', (2, -1)), (',', (3, -1))
    , ('.', (4, -1)), ('/', (5, -1))

    , ('b', (0, -1)), ('v', (-1, -1)), ('c', (-2, -1))
    , ('x', (-3, -1)), ('z', (-4, -1))

    , ('6', (1, 2)), ('7', (2, 2)), ('8', (3, 2))
    , ('9', (4, 2)), ('0', (5, 2)), ('-', (6, 2))
    , ('=', (7, 2))

    , ('5', (-1, 2)), ('4', (-2, 2)), ('3', (-3, 2))
    , ('2', (-4, 2)), ('1', (-5, 2)), ('`', (-6, 2))
    ]

qwertyShifted :: KeyCoords
qwertyShifted = M.mapKeys (M.! qwertyShifted) qwertyUnshifted

dvorakCoords :: KeyCoords
dvorakCoords = M.fromList $ []
