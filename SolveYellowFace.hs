{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module SolveYellowFace where

import Debug.Trace
import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

{-- Phase 4
solve Yellow face --}

solveYellowFace :: CubeWithMoves -> CubeWithMoves
solveYellowFace cube = solveYellowCorners $ solveYellowCross cube

{-- Phase 4.1
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

{-- Phase 4.2
solve Yellow corners --}

solveYellowCorners :: CubeWithMoves -> CubeWithMoves
solveYellowCorners cube = if checkYellowFace cube then cube else solveYellowCorners $ fixYellowCorners cube

fixYellowCorners :: CubeWithMoves -> CubeWithMoves
fixYellowCorners cube
  | down !! 2 == Yellow && (countYellowCorners cube == 1) = trace ("DEBUG" ++ show cube ++ "\n" ++ show (countYellowCorners cube)) $ makeMoveAndNoteWhiteDown Front [R, U, R', U, R, U, U, R'] cube
  | right !! 6 == Yellow && (countYellowCorners cube == 0) = trace ("DEBUG" ++ show cube ++ "\n" ++ show (countYellowCorners cube)) $ makeMoveAndNoteWhiteDown Front [R, U, R', U, R, U, U, R'] cube
  | front !! 8 == Yellow && (countYellowCorners cube == 2) = trace ("DEBUG" ++ show cube ++ "\n" ++ show (countYellowCorners cube)) $ makeMoveAndNoteWhiteDown Front [R, U, R', U, R, U, U, R'] cube
  | otherwise = trace ("DEBUG" ++ show cube ++ "\n" ++ show (countYellowCorners cube) ++ "\n" ++ show (up !! 6)) $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    (front, left, back, right, up, down) = getSides (fst cube)

checkYellowFace :: CubeWithMoves -> Bool
checkYellowFace cube =
  down !! 0 == Yellow
    && down !! 1 == Yellow
    && down !! 2 == Yellow
    && down !! 3 == Yellow
    && down !! 5 == Yellow
    && down !! 6 == Yellow
    && down !! 7 == Yellow
    && down !! 8 == Yellow
  where
    down = getSide Down (fst cube)

countYellowCorners :: CubeWithMoves -> Int
countYellowCorners cube = foldl (\acc index -> if down !! index == Yellow then acc + 1 else acc) 0 [0, 2, 6, 8]
  where
    down = getSide Down (fst cube)