#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package deque-0.4.4.1 --package safe-0.3.19

module WalkableBoundedWorld where

-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, sortBy, groupBy, delete, find, transpose, findIndex)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes, fromMaybe)
import Linear hiding (trace, transpose)
import Linear.V2
import Debug.Trace (trace)
import Data.Ord
import Data.Function
import Data.Tuple
import GHC.Exts as F
import Data.Bits
import Control.Monad (guard, join)
import Data.Monoid
import Data.Foldable
import Data.Ord
import Safe (atMay)

import Util (replace, iterate')
import Layer (pointToIndex, pointToLayer, moveLayer, movePoint, isOverlapping, diff, up, dn, lt, rt, allDirs)
import World (SingularPoint, Layer, World(..), emptyWorld, readWorld, showWorld, printWorld, combineTwoWorlds, combineWorlds, hasPoint, moveLayerInWorld, movePointInWorld, cutLayerWithLayer, setPoint, insertLayerAtPoint, isOverlappingLayers)

{- ADD STUFF -}