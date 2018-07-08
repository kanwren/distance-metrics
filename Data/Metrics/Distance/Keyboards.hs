module Data.Metrics.Distance.Keyboards (
    EditCosts,
    damerauLevenshtein,
    defaultEditCosts,
    dvorakEditCosts,
    euclideanKeyboardCosts,
    hamming,
    levenshtein,
    optimalStringAlignment,
    qwertyEditCosts,
    ) where

import Data.Map as M

data EditCosts 
