module Data.Metrics.Distance.Keyboards.Dvorak (
    dvorakCoords
    ) where

import qualified Data.Map.Strict as M

dvorakCoords :: M.Map Char (Int, Int)
dvorakCoords = M.union dvorakUnshifted dvorakShifted

dvorakUnshifted :: M.Map Char (Int, Int)
dvorakUnshifted = M.fromList undefined

dvorakShifted :: M.Map Char (Int, Int)
dvorakShifted = M.mapKeys (dvorakShift M.!) dvorakUnshifted

dvorakShift :: M.Map Char Char
dvorakShift = M.fromList undefined
