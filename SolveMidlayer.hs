{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module SolveMidlayer where

import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

{-- Phase 3
mid layer
--}

solveMidLayer :: CubeWithMoves -> CubeWithMoves
solveMidLayer cube = if checkMidLayer cube then cube else solveMidLayer $ fixMidLayer cube

checkMidLayer :: CubeWithMoves -> Bool
checkMidLayer (cube, _) =
  front !! 3 == Red && front !! 5 == Red
    && left !! 3 == Green
    && left !! 5 == Green
    && back !! 3 == Orange
    && back !! 5 == Orange
    && right !! 3 == Blue
    && right !! 5 == Blue
  where
    (front, left, back, right, _, _) = getSides cube

fixMidLayer :: CubeWithMoves -> CubeWithMoves
fixMidLayer cube = moveInvalidFromSecondLayerFront $ fixLeftMidLayer $ fixBackMidLayer $ fixRightMidLayer $ fixFrontMidLayer cube

moveInvalidFromSecondLayerFront :: CubeWithMoves -> CubeWithMoves
moveInvalidFromSecondLayerFront cube
  | front !! 3 /= Red || left !! 5 /= Green = fixFrontMidLayer $ makeMoveAndNoteWhiteDown Front [U, R, U', R', U', F', U, F] cube
  | otherwise = moveInvalidFromSecondLayerRight cube
  where
    (front, left, back, right, _, _) = getSides (fst cube)

moveInvalidFromSecondLayerRight :: CubeWithMoves -> CubeWithMoves
moveInvalidFromSecondLayerRight cube
  | front !! 5 /= Red || right !! 3 /= Blue = fixRightMidLayer $ makeMoveAndNoteWhiteDown Right [U, R, U', R', U', F', U, F] cube
  | otherwise = moveInvalidFromSecondLayerBack cube
  where
    (front, left, back, right, _, _) = getSides (fst cube)

moveInvalidFromSecondLayerBack :: CubeWithMoves -> CubeWithMoves
moveInvalidFromSecondLayerBack cube
  | right !! 5 /= Blue || back !! 3 /= Orange = fixBackMidLayer $ makeMoveAndNoteWhiteDown Back [U, R, U', R', U', F', U, F] cube
  | otherwise = moveInvalidFromSecondLayerLeft cube
  where
    (front, left, back, right, _, _) = getSides (fst cube)

moveInvalidFromSecondLayerLeft :: CubeWithMoves -> CubeWithMoves
moveInvalidFromSecondLayerLeft cube
  | back !! 5 /= Orange || left !! 3 /= Green = fixLeftMidLayer $ makeMoveAndNoteWhiteDown Left [U, R, U', R', U', F', U, F] cube
  | otherwise = cube
  where
    (front, left, back, right, _, _) = getSides (fst cube)

fixFrontMidLayer :: CubeWithMoves -> CubeWithMoves
fixFrontMidLayer cube
  | front !! 7 /= Yellow && down !! 1 /= Yellow = fixMidLayerFront cube
  | otherwise = cube
  where
    front = getSide Front (fst cube)
    down = getSide Down (fst cube)

fixRightMidLayer :: CubeWithMoves -> CubeWithMoves
fixRightMidLayer cube
  | right !! 7 /= Yellow && down !! 5 /= Yellow = fixMidLayerRight cube
  | otherwise = cube
  where
    right = getSide Right (fst cube)
    down = getSide Down (fst cube)

fixBackMidLayer :: CubeWithMoves -> CubeWithMoves
fixBackMidLayer cube
  | back !! 7 /= Yellow && down !! 7 /= Yellow = fixMidLayerBack cube
  | otherwise = cube
  where
    back = getSide Back (fst cube)
    down = getSide Down (fst cube)

fixLeftMidLayer :: CubeWithMoves -> CubeWithMoves
fixLeftMidLayer cube
  | left !! 7 /= Yellow && down !! 3 /= Yellow = fixMidLayerLeft cube
  | otherwise = cube
  where
    left = getSide Left (fst cube)
    down = getSide Down (fst cube)

fixMidLayerFront :: CubeWithMoves -> CubeWithMoves
fixMidLayerFront cube
  | front !! 7 == Red && down !! 1 == Blue = makeMoveAndNoteWhiteDown Front [U', L', U, L, U, F, U', F'] cube -- Right
  | front !! 7 == Red && down !! 1 == Green = makeMoveAndNoteWhiteDown Front [U, R, U', R', U', F', U, F] cube -- Left
  | otherwise = fixMidLayerRight $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    front = getSide Front (fst cube)
    down = getSide Down (fst cube)

fixMidLayerRight :: CubeWithMoves -> CubeWithMoves
fixMidLayerRight cube
  | right !! 7 == Blue && down !! 5 == Orange = makeMoveAndNoteWhiteDown Right [U', L', U, L, U, F, U', F'] cube
  | right !! 7 == Blue && down !! 5 == Red = makeMoveAndNoteWhiteDown Right [U, R, U', R', U', F', U, F] cube
  | otherwise = fixMidLayerBack $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    right = getSide Right (fst cube)
    down = getSide Down (fst cube)

fixMidLayerBack :: CubeWithMoves -> CubeWithMoves
fixMidLayerBack cube
  | back !! 7 == Orange && down !! 7 == Green = makeMoveAndNoteWhiteDown Back [U', L', U, L, U, F, U', F'] cube
  | back !! 7 == Orange && down !! 7 == Blue = makeMoveAndNoteWhiteDown Back [U, R, U', R', U', F', U, F] cube
  | otherwise = fixMidLayerLeft $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    back = getSide Back (fst cube)
    down = getSide Down (fst cube)

fixMidLayerLeft :: CubeWithMoves -> CubeWithMoves
fixMidLayerLeft cube
  | left !! 7 == Green && down !! 3 == Red = makeMoveAndNoteWhiteDown Left [U', L', U, L, U, F, U', F'] cube
  | left !! 7 == Green && down !! 3 == Orange = makeMoveAndNoteWhiteDown Left [U, R, U', R', U', F', U, F] cube
  | otherwise = fixMidLayerFront $ makeMoveAndNoteWhiteUp Front [D] cube
  where
    left = getSide Left (fst cube)
    down = getSide Down (fst cube)
