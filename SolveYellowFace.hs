{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module SolveYellowFace where

import Debug.Trace
import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

{-- Phase 4
solve white cross --}

solveYellowCross :: CubeWithMoves -> CubeWithMoves
solveYellowCross cube = if checkYellowCross cube then cube else solveYellowCross $ fixYellowCross cube

checkYellowCross :: CubeWithMoves -> Bool
checkYellowCross cube =
  down !! 1 == Yellow
    && down !! 3 == Yellow
    && down !! 5 == Yellow
    && down !! 7 == Yellow
  where
    down = getSide Down (fst cube)

fixYellowCross :: CubeWithMoves -> CubeWithMoves
fixYellowCross cube
  | down !! 1 == Yellow && down !! 3 == Yellow = makeMoveAndNoteWhiteDown Back [F, U, R, U', R', F'] cube
  | down !! 1 == Yellow && down !! 5 == Yellow = makeMoveAndNoteWhiteDown Left [F, U, R, U', R', F'] cube
  | down !! 5 == Yellow && down !! 7 == Yellow = makeMoveAndNoteWhiteDown Front [F, U, R, U', R', F'] cube
  | down !! 3 == Yellow && down !! 7 == Yellow = makeMoveAndNoteWhiteDown Right [F, U, R, U', R', F'] cube
  | down !! 1 == Yellow && down !! 7 == Yellow = makeMoveAndNoteWhiteDown Left [F, U, R, U', R', F'] cube
  | down !! 3 == Yellow && down !! 5 == Yellow = makeMoveAndNoteWhiteDown Front [F, U, R, U', R', F'] cube
  | otherwise = makeMoveAndNoteWhiteDown Front [F, U, R, U', R', F'] cube
  where
    down = getSide Down (fst cube)