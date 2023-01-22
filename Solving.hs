module Solving where

import Debug.Trace
import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

makeMoveAndNoteWhiteUp :: Side -> [Move] -> (Cube, [Move]) -> (Cube, [Move])
makeMoveAndNoteWhiteUp side moves (cube, past_moves) =
  trace
    ("DEBUG" ++ show moves ++ show side)
    (makeMoves translated_moves cube, past_moves ++ translated_moves)
  where
    translated_moves = translateMovesWhiteUp side moves

makeMoveAndNoteWhiteDown :: Side -> [Move] -> (Cube, [Move]) -> (Cube, [Move])
makeMoveAndNoteWhiteDown side moves (cube, past_moves) =
  (makeMoves translated_moves cube, past_moves ++ translated_moves)
  where
    translated_moves = translateMovesWhiteDown side moves

{-- First step, solve white cross --}

solveWhiteCross :: (Cube, [Move]) -> (Cube, [Move])
solveWhiteCross cube = if checkCrossAndSides (fst cube) then cube else solveWhiteCross $ fixUpEdges cube

checkCross :: Cube -> Side -> Bool
checkCross cube side =
  colors !! 1 == getTargetSideColor side
    && colors !! 3 == getTargetSideColor side
    && colors !! 5 == getTargetSideColor side
    && colors !! 7 == getTargetSideColor side
  where
    colors = getSide side cube

checkCrossAndSides :: Cube -> Bool
checkCrossAndSides cube =
  ( front !! 1 == getTargetSideColor Front
      && right !! 1 == getTargetSideColor Right
      && left !! 1 == getTargetSideColor Left
      && back !! 1 == getTargetSideColor Back
  )
    && checkCross cube Up
  where
    (front, left, back, right, up, down) = getSides cube

fixUpEdges :: (Cube, [Move]) -> (Cube, [Move])
fixUpEdges cube = fixBackUpEdges $ fixLeftUpEdges $ fixRightUpEdges $ fixFrontUpEdges cube

fixFrontUpEdges :: (Cube, [Move]) -> (Cube, [Move])
fixFrontUpEdges cube
  | front !! 1 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontWhiteSideBottom $ makeMoveAndNoteWhiteUp Front [F, F] cube
  | front !! 3 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontWhiteSideBottom $ makeMoveAndNoteWhiteUp Front [F'] cube
  | front !! 5 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontWhiteSideBottom $ makeMoveAndNoteWhiteUp Front [F] cube
  | front !! 7 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontWhiteSideBottom cube
  | getSide Down (fst cube) !! 1 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontWhiteDown cube
  | otherwise = cube
  where
    front = getSide Front (fst cube)

fixRightUpEdges :: (Cube, [Move]) -> (Cube, [Move])
fixRightUpEdges cube
  | right !! 1 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightWhiteSideBottom $ makeMoveAndNoteWhiteUp Right [F, F] cube
  | right !! 3 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightWhiteSideBottom $ makeMoveAndNoteWhiteUp Right [F'] cube
  | right !! 5 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightWhiteSideBottom $ makeMoveAndNoteWhiteUp Right [F] cube
  | right !! 7 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightWhiteSideBottom cube
  | getSide Down (fst cube) !! 5 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightWhiteDown cube
  | otherwise = cube
  where
    right = getSide Right (fst cube)

fixLeftUpEdges :: (Cube, [Move]) -> (Cube, [Move])
fixLeftUpEdges cube
  | left !! 1 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftWhiteSideBottom $ makeMoveAndNoteWhiteUp Left [F, F] cube
  | left !! 3 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftWhiteSideBottom $ makeMoveAndNoteWhiteUp Left [F'] cube
  | left !! 5 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftWhiteSideBottom $ makeMoveAndNoteWhiteUp Left [F] cube
  | left !! 7 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftWhiteSideBottom cube
  | getSide Down (fst cube) !! 3 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftWhiteDown cube
  | otherwise = cube
  where
    left = getSide Left (fst cube)

fixBackUpEdges :: (Cube, [Move]) -> (Cube, [Move])
fixBackUpEdges cube
  | back !! 1 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackWhiteSideBottom $ makeMoveAndNoteWhiteUp Back [F, F] cube
  | back !! 3 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackWhiteSideBottom $ makeMoveAndNoteWhiteUp Back [F'] cube
  | back !! 5 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackWhiteSideBottom $ makeMoveAndNoteWhiteUp Back [F] cube
  | back !! 7 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackWhiteSideBottom cube
  | getSide Down (fst cube) !! 7 == getTargetSideColor Up = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackWhiteDown cube
  | otherwise = cube
  where
    back = getSide Back (fst cube)

fixFrontWhiteSideBottom :: (Cube, [Move]) -> (Cube, [Move])
fixFrontWhiteSideBottom cube
  | getSide Down (fst cube) !! 1 == getTargetSideColor Front = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D', L', F, L] cube
  | otherwise = trace ("DEBUG fixFrontWhiteSideBottom" ++ show cube) fixRightWhiteSideBottom $ makeMoveAndNoteWhiteUp Front [D] cube

fixRightWhiteSideBottom :: (Cube, [Move]) -> (Cube, [Move])
fixRightWhiteSideBottom cube
  | getSide Down (fst cube) !! 5 == getTargetSideColor Right = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D', L', F, L] cube
  | otherwise = trace ("DEBUG fixRightWhiteSideBottom" ++ show (getSide Down (fst cube) !! 5) ++ show cube) $ fixBackWhiteSideBottom $ makeMoveAndNoteWhiteUp Front [D] cube

fixBackWhiteSideBottom :: (Cube, [Move]) -> (Cube, [Move])
fixBackWhiteSideBottom cube
  | getSide Down (fst cube) !! 7 == getTargetSideColor Back = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [D', L', F, L] cube
  | otherwise = trace ("DEBUG fixBackWhiteSideBottom" ++ show cube) $ fixLeftWhiteSideBottom $ makeMoveAndNoteWhiteUp Front [D] cube

fixLeftWhiteSideBottom :: (Cube, [Move]) -> (Cube, [Move])
fixLeftWhiteSideBottom cube
  | getSide Down (fst cube) !! 3 == getTargetSideColor Left = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [D', L', F, L] cube
  | otherwise = trace ("DEBUG fixLeftWhiteSideBottom" ++ show cube) $ fixFrontWhiteSideBottom $ makeMoveAndNoteWhiteUp Front [D] cube

fixFrontWhiteDown :: (Cube, [Move]) -> (Cube, [Move])
fixFrontWhiteDown cube
  | getSide Front (fst cube) !! 7 == getTargetSideColor Front = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixRightWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube

fixRightWhiteDown :: (Cube, [Move]) -> (Cube, [Move])
fixRightWhiteDown cube
  | getSide Right (fst cube) !! 7 == getTargetSideColor Right = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixBackWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube

fixBackWhiteDown :: (Cube, [Move]) -> (Cube, [Move])
fixBackWhiteDown cube
  | getSide Back (fst cube) !! 7 == getTargetSideColor Back = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixLeftWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube

fixLeftWhiteDown :: (Cube, [Move]) -> (Cube, [Move])
fixLeftWhiteDown cube
  | getSide Left (fst cube) !! 7 == getTargetSideColor Left = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixFrontWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
