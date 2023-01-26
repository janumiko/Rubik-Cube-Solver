{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Solving where

import Debug.Trace
import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

import SolveMidlayer
import SolveWhiteCorners
import SolveWhiteCross

solveCube :: Cube -> CubeWithMoves
solveCube cube = solveMidLayer $ solveWhiteCorners $ solveWhiteCross (cube, [])