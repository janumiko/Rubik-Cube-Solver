{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module SolveWhiteCross where

import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

{-- Phase 1
solve white cross --}

solveWhiteCross :: CubeWithMoves -> CubeWithMoves
solveWhiteCross cube = if checkCrossAndSides (fst cube) then cube else solveWhiteCross $ fixUpSides cube

checkWhiteCross :: Cube -> Side -> Bool
checkWhiteCross cube side =
  up !! 1 == White
    && up !! 3 == White
    && up !! 5 == White
    && up !! 7 == White
  where
    up = getSide Up cube

checkCrossAndSides :: Cube -> Bool
checkCrossAndSides cube =
  ( front !! 1 == Red
      && right !! 1 == Blue
      && left !! 1 == Green
      && back !! 1 == Orange
  )
    && checkWhiteCross cube Up
  where
    (front, left, back, right, up, down) = getSides cube

fixUpSides :: CubeWithMoves -> CubeWithMoves
fixUpSides cube = fixBackUpSides $ fixLeftUpSides $ fixRightUpSides $ fixFrontUpSides cube

fixFrontUpSides :: CubeWithMoves -> CubeWithMoves
fixFrontUpSides cube
  | front !! 1 == White = fixFrontUpSides $ fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [F, F] cube
  | front !! 3 == White = fixFrontUpSides $ fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [F'] cube
  | front !! 5 == White = fixFrontUpSides $ fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [F] cube
  | front !! 7 == White = fixFrontUpSides $ fixFrontWhiteSideDown cube
  | down !! 1 == White = fixFrontUpSides $ fixFrontWhiteDown cube
  | otherwise = cube
  where
    front = getSide Front (fst cube)
    down = getSide Down (fst cube)

fixRightUpSides :: CubeWithMoves -> CubeWithMoves
fixRightUpSides cube
  | right !! 1 == White = fixRightUpSides $ fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Right [F, F] cube
  | right !! 3 == White = fixRightUpSides $ fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Right [F'] cube
  | right !! 5 == White = fixRightUpSides $ fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Right [F] cube
  | right !! 7 == White = fixRightUpSides $ fixRightWhiteSideDown cube
  | down !! 5 == White = fixRightUpSides $ fixRightWhiteDown cube
  | otherwise = cube
  where
    right = getSide Right (fst cube)
    down = getSide Down (fst cube)

fixLeftUpSides :: CubeWithMoves -> CubeWithMoves
fixLeftUpSides cube
  | left !! 1 == White = fixLeftUpSides $ fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Left [F, F] cube
  | left !! 3 == White = fixLeftUpSides $ fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Left [F'] cube
  | left !! 5 == White = fixLeftUpSides $ fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Left [F] cube
  | left !! 7 == White = fixLeftUpSides $ fixLeftWhiteSideDown cube
  | down !! 3 == White = fixLeftUpSides $ fixLeftWhiteDown cube
  | otherwise = cube
  where
    left = getSide Left (fst cube)
    down = getSide Down (fst cube)

fixBackUpSides :: CubeWithMoves -> CubeWithMoves
fixBackUpSides cube
  | back !! 1 == White = fixBackUpSides $ fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Back [F, F] cube
  | back !! 3 == White = fixBackUpSides $ fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Back [F'] cube
  | back !! 5 == White = fixBackUpSides $ fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Back [F] cube
  | back !! 7 == White = fixBackUpSides $ fixBackWhiteSideDown cube
  | down !! 7 == White = fixBackUpSides $ fixBackWhiteDown cube
  | otherwise = cube
  where
    back = getSide Back (fst cube)
    down = getSide Down (fst cube)

fixFrontWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixFrontWhiteSideDown cube
  | down !! 1 == Red = makeMoveAndNoteWhiteUp Front [D', L', F, L] cube
  | otherwise = fixRightWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    down = getSide Down (fst cube)

fixRightWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixRightWhiteSideDown cube
  | down !! 5 == Blue = makeMoveAndNoteWhiteUp Right [D', L', F, L] cube
  | otherwise = fixBackWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    down = getSide Down (fst cube)

fixBackWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixBackWhiteSideDown cube
  | down !! 7 == Orange = makeMoveAndNoteWhiteUp Back [D', L', F, L] cube
  | otherwise = fixLeftWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    down = getSide Down (fst cube)

fixLeftWhiteSideDown :: CubeWithMoves -> CubeWithMoves
fixLeftWhiteSideDown cube
  | down !! 3 == Green = makeMoveAndNoteWhiteUp Left [D', L', F, L] cube
  | otherwise = fixFrontWhiteSideDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    down = getSide Down (fst cube)

fixFrontWhiteDown :: CubeWithMoves -> CubeWithMoves
fixFrontWhiteDown cube
  | front !! 7 == Red = makeMoveAndNoteWhiteUp Front [F, F] cube
  | otherwise = fixRightWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    front = getSide Front (fst cube)

fixRightWhiteDown :: CubeWithMoves -> CubeWithMoves
fixRightWhiteDown cube
  | right !! 7 == Blue = makeMoveAndNoteWhiteUp Right [F, F] cube
  | otherwise = fixBackWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    right = getSide Right (fst cube)

fixBackWhiteDown :: CubeWithMoves -> CubeWithMoves
fixBackWhiteDown cube
  | back !! 7 == Orange = makeMoveAndNoteWhiteUp Back [F, F] cube
  | otherwise = fixLeftWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    back = getSide Back (fst cube)

fixLeftWhiteDown :: CubeWithMoves -> CubeWithMoves
fixLeftWhiteDown cube
  | left !! 7 == Green = makeMoveAndNoteWhiteUp Left [F, F] cube
  | otherwise = fixFrontWhiteDown $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    left = getSide Left (fst cube)