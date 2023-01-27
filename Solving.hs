{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Solving where

import Debug.Trace
import Rotations
import SolveMidlayer
import SolveWhiteCorners
import SolveWhiteCross
import SolveYellowFace
import Types
import Utils
import Prelude hiding (Left, Right)

solveCube :: Cube -> CubeWithMoves
solveCube cube = solveYellowCross $ solveMidLayer $ solveWhiteCorners $ solveWhiteCross (cube, [])