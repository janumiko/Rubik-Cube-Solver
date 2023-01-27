{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module SolveWhiteCorners where

import Debug.Trace
import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

{-- Phase 2
solve white corners --}

solveWhiteCorners :: CubeWithMoves -> CubeWithMoves
solveWhiteCorners cube = if checkWhiteCornersAndSides cube then cube else solveWhiteCorners $ fixUpWhiteEdges cube

checkWhiteCornersAndSides :: CubeWithMoves -> Bool
checkWhiteCornersAndSides cube =
  checkWhiteUp cube
    && checkSide Front front
    && checkSide Right right
    && checkSide Left left
    && checkSide Back back
  where
    (front, left, back, right, up, down) = getSides (fst cube)
    checkSide side colors = colors !! 0 == getTargetSideColor side && colors !! 2 == getTargetSideColor side

checkWhiteUp :: CubeWithMoves -> Bool
checkWhiteUp (cube, moves) = up !! 0 == White && up !! 2 == White && up !! 6 == White && up !! 8 == White
  where
    up = getSide Up cube

fixUpWhiteEdges :: CubeWithMoves -> CubeWithMoves
fixUpWhiteEdges cube = fixBackUpEdges $ fixLeftUpEdges $ fixRightUpEdges $ fixFrontUpEdges $ takeDownWrongEdges cube

takeDownWrongEdges :: CubeWithMoves -> CubeWithMoves
takeDownWrongEdges cube
  | up !! 6 == White && (front !! 0 /= Red || left !! 2 /= Green) = trace ("DEBUG" ++ show cube) $ takeDownWrongEdges $ makeMoveAndNoteWhiteUp Front [L, D, L'] cube
  | up !! 8 == White && (front !! 2 /= Red || right !! 0 /= Blue) = trace ("DEBUG" ++ show cube) $ takeDownWrongEdges $ makeMoveAndNoteWhiteUp Right [L, D, L'] cube
  | up !! 0 == White && (back !! 0 /= Orange || left !! 0 /= Green) = trace ("DEBUG" ++ show cube) $ takeDownWrongEdges $ makeMoveAndNoteWhiteUp Left [L, D, L'] cube
  | up !! 2 == White && (right !! 2 /= Blue || back !! 0 /= Orange) = trace ("DEBUG" ++ show cube) $ takeDownWrongEdges $ makeMoveAndNoteWhiteUp Back [L, D, L'] cube
  | otherwise = cube
  where
    (front, left, back, right, up, down) = getSides (fst cube)

fixFrontUpEdges :: CubeWithMoves -> CubeWithMoves
fixFrontUpEdges cube
  | front !! 0 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontLeftDownEdge $ makeMoveAndNoteWhiteUp Front [F', D', F, D] cube
  | front !! 2 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontRightDownEdge $ makeMoveAndNoteWhiteUp Front [F, D, F', D'] cube
  | front !! 6 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontLeftDownEdge cube
  | front !! 8 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ fixFrontRightDownEdge cube
  | down !! 0 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ moveFromDownToSideFront cube
  | down !! 2 == White = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ makeMoveAndNoteWhiteUp Front [D'] cube
  | otherwise = trace ("DEBUG" ++ show cube) cube
  where
    front = getSide Front (fst cube)
    down = getSide Down (fst cube)

fixRightUpEdges :: CubeWithMoves -> CubeWithMoves
fixRightUpEdges cube
  | right !! 0 == White = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightLeftDownEdge $ makeMoveAndNoteWhiteUp Right [F', D', F, D] cube
  | right !! 2 == White = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightRightDownEdge $ makeMoveAndNoteWhiteUp Right [F, D, F', D'] cube
  | right !! 6 == White = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightLeftDownEdge cube
  | right !! 8 == White = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ fixRightRightDownEdge cube
  | down !! 2 == White = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ moveFromDownToSideRight cube
  | down !! 8 == White = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ makeMoveAndNoteWhiteUp Right [D'] cube
  | otherwise = trace ("DEBUG" ++ show cube) cube
  where
    right = getSide Right (fst cube)
    down = getSide Down (fst cube)

fixLeftUpEdges :: CubeWithMoves -> CubeWithMoves
fixLeftUpEdges cube
  | left !! 0 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftLeftDownEdge $ makeMoveAndNoteWhiteUp Left [F', D', F, D] cube
  | left !! 2 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftRightDownEdge $ makeMoveAndNoteWhiteUp Left [F, D, F', D'] cube
  | left !! 6 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftLeftDownEdge cube
  | left !! 8 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ fixLeftRightDownEdge cube
  | down !! 6 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ moveFromDownToSideLeft cube
  | down !! 0 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ makeMoveAndNoteWhiteUp Left [D'] cube
  | otherwise = trace ("DEBUG" ++ show cube) cube
  where
    left = getSide Left (fst cube)
    down = getSide Down (fst cube)

fixBackUpEdges :: CubeWithMoves -> CubeWithMoves
fixBackUpEdges cube
  | back !! 0 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackLeftDownEdge $ makeMoveAndNoteWhiteUp Back [F', D', F, D] cube
  | back !! 2 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackRightDownEdge $ makeMoveAndNoteWhiteUp Back [F, D, F', D'] cube
  | back !! 6 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackLeftDownEdge cube
  | back !! 8 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ fixBackRightDownEdge cube
  | down !! 8 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ moveFromDownToSideBack cube
  | down !! 6 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ makeMoveAndNoteWhiteUp Back [D'] cube
  | otherwise = trace ("DEBUG" ++ show cube) cube
  where
    back = getSide Back (fst cube)
    down = getSide Down (fst cube)

moveFromDownToSideFront :: CubeWithMoves -> CubeWithMoves
moveFromDownToSideFront cube
  | left !! 8 == Red && front !! 6 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) moveFromDownToSideRight $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    front = getSide Front (fst cube)
    left = getSide Left (fst cube)

moveFromDownToSideRight :: CubeWithMoves -> CubeWithMoves
moveFromDownToSideRight cube
  | front !! 8 == Blue && right !! 6 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) moveFromDownToSideBack $ makeMoveAndNoteWhiteUp Right [D] cube
  where
    front = getSide Front (fst cube)
    right = getSide Right (fst cube)

moveFromDownToSideBack :: CubeWithMoves -> CubeWithMoves
moveFromDownToSideBack cube
  | right !! 8 == Orange && back !! 6 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) moveFromDownToSideLeft $ makeMoveAndNoteWhiteUp Back [D] cube
  where
    right = getSide Right (fst cube)
    back = getSide Back (fst cube)

moveFromDownToSideLeft :: CubeWithMoves -> CubeWithMoves
moveFromDownToSideLeft cube
  | back !! 8 == Green && left !! 6 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) moveFromDownToSideFront $ makeMoveAndNoteWhiteUp Left [D] cube
  where
    back = getSide Back (fst cube)
    left = getSide Left (fst cube)

fixFrontLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixFrontLeftDownEdge cube
  | down !! 0 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixRightLeftDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    down = getSide Down (fst cube)

fixRightLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixRightLeftDownEdge cube
  | down !! 2 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixBackLeftDownEdge $ makeMoveAndNoteWhiteUp Right [D] cube
  where
    down = getSide Down (fst cube)

fixBackLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixBackLeftDownEdge cube
  | down !! 8 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixLeftLeftDownEdge $ makeMoveAndNoteWhiteUp Back [D] cube
  where
    down = getSide Down (fst cube)

fixLeftLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixLeftLeftDownEdge cube
  | down !! 6 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixFrontLeftDownEdge $ makeMoveAndNoteWhiteUp Left [D] cube
  where
    down = getSide Down (fst cube)

fixFrontRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixFrontRightDownEdge cube
  | down !! 2 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixRightRightDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    down = getSide Down (fst cube)

fixRightRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixRightRightDownEdge cube
  | down !! 8 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixBackRightDownEdge $ makeMoveAndNoteWhiteUp Right [D] cube
  where
    down = getSide Down (fst cube)

fixBackRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixBackRightDownEdge cube
  | down !! 6 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixLeftRightDownEdge $ makeMoveAndNoteWhiteUp Back [D] cube
  where
    down = getSide Down (fst cube)

fixLeftRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixLeftRightDownEdge cube
  | down !! 0 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixFrontRightDownEdge $ makeMoveAndNoteWhiteUp Left [D] cube
  where
    down = getSide Down (fst cube)