{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

module SolveWhiteCross
where
    
import Debug.Trace
import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

{-- Phase 1
solve white cross --}

solveWhiteCross :: CubeWithMoves -> CubeWithMoves
solveWhiteCross cube = if checkCrossAndSides (fst cube) then cube else solveWhiteCross $ fixUpSides cube

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
  ( front !! 1 == Red
      && right !! 1 == Blue
      && left !! 1 == Green
      && back !! 1 == Orange
  )
    && checkCross cube Up
  where
    (front, left, back, right, up, down) = getSides cube

fixUpSides :: CubeWithMoves -> CubeWithMoves
fixUpSides cube = fixBackUpSides $ fixLeftUpSides $ fixRightUpSides $ fixFrontUpSides cube

fixFrontUpSides :: CubeWithMoves -> CubeWithMoves
fixFrontUpSides cube
  | front !! 1 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpSides $ fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [F, F] cube
  | front !! 3 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpSides $ fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [F'] cube
  | front !! 5 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpSides $ fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [F] cube
  | front !! 7 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpSides $ fixFrontWhiteSideDown cube
  | down !! 1 == White  = trace ("DEBUG" ++ show cube) $ fixFrontUpSides $ fixFrontWhiteDown cube
  | otherwise = cube
  where
    front = getSide Front (fst cube)
    down = getSide Down (fst cube)

fixRightUpSides :: CubeWithMoves -> CubeWithMoves
fixRightUpSides cube
  | right !! 1 == White = trace ("DEBUG" ++ show cube) $ fixRightUpSides $ fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Right [F, F] cube
  | right !! 3 == White = trace ("DEBUG" ++ show cube) $ fixRightUpSides $ fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Right [F'] cube
  | right !! 5 == White = trace ("DEBUG" ++ show cube) $ fixRightUpSides $ fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Right [F] cube
  | right !! 7 == White = trace ("DEBUG" ++ show cube) $ fixRightUpSides $ fixRightWhiteSideDown cube
  | down !! 5 == White  = trace ("DEBUG" ++ show cube) $ fixRightUpSides $ fixRightWhiteDown cube
  | otherwise = cube
  where
    right = getSide Right (fst cube)
    down = getSide Down (fst cube)

fixLeftUpSides :: CubeWithMoves -> CubeWithMoves
fixLeftUpSides cube
  | left !! 1 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpSides $ fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Left [F, F] cube
  | left !! 3 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpSides $ fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Left [F'] cube
  | left !! 5 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpSides $ fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Left [F] cube
  | left !! 7 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpSides $ fixLeftWhiteSideDown cube
  | down !! 3 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpSides $ fixLeftWhiteDown cube
  | otherwise = cube
  where
    left = getSide Left (fst cube)
    down = getSide Down (fst cube)

fixBackUpSides :: CubeWithMoves -> CubeWithMoves
fixBackUpSides cube
  | back !! 1 == White = trace ("DEBUG" ++ show cube) $ fixBackUpSides $ fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Back [F, F] cube
  | back !! 3 == White = trace ("DEBUG" ++ show cube) $ fixBackUpSides $ fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Back [F'] cube
  | back !! 5 == White = trace ("DEBUG" ++ show cube) $ fixBackUpSides $ fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Back [F] cube
  | back !! 7 == White = trace ("DEBUG" ++ show cube) $ fixBackUpSides $ fixBackWhiteSideDown cube
  | down !! 7 == White = trace ("DEBUG" ++ show cube) $ fixBackUpSides $ fixBackWhiteDown cube
  | otherwise = cube
  where
    back = getSide Back (fst cube)
    down = getSide Down (fst cube)

fixFrontWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixFrontWhiteSideDown cube
  | down !! 1 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D', L', F, L] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixRightWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixRightWhiteSideDown cube
  | down !! 5 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D', L', F, L] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixBackWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixBackWhiteSideDown cube
  | down !! 7 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [D', L', F, L] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixLeftWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixLeftWhiteSideDown cube
  | down !! 3 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [D', L', F, L] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixFrontWhiteDown :: CubeWithMoves -> CubeWithMoves
fixFrontWhiteDown cube
  | front !! 7 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixRightWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where front = getSide Front (fst cube)

fixRightWhiteDown :: CubeWithMoves -> CubeWithMoves
fixRightWhiteDown cube
  | right !! 7 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixBackWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where right = getSide Right (fst cube)

fixBackWhiteDown :: CubeWithMoves -> CubeWithMoves
fixBackWhiteDown cube
  | back !! 7 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixLeftWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where back = getSide Back (fst cube)

fixLeftWhiteDown :: CubeWithMoves -> CubeWithMoves
fixLeftWhiteDown cube
  | left !! 7 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [F, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixFrontWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where left = getSide Left (fst cube)