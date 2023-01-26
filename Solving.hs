{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Solving where

import Debug.Trace
import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

solveCube :: Cube -> CubeWithMoves
solveCube cube = solveMidLayer $ solveWhiteCorners $ solveWhiteCross (cube, [])

makeMoveAndNoteWhiteUp :: Side -> [Move] -> CubeWithMoves -> CubeWithMoves
makeMoveAndNoteWhiteUp side moves (cube, past_moves) =
  trace
    ("DEBUG" ++ show moves ++ show side)
    (makeMoves translated_moves cube, past_moves ++ translated_moves)
  where
    translated_moves = translateMovesWhiteUp side moves

makeMoveAndNoteWhiteDown :: Side -> [Move] -> CubeWithMoves -> CubeWithMoves
makeMoveAndNoteWhiteDown side moves (cube, past_moves) =
  trace
    ("DEBUG" ++ show moves ++ show side)
    (makeMoves translated_moves cube, past_moves ++ translated_moves)
  where
    translated_moves = translateMovesWhiteDown side moves

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

{-- Phase 2
solve white corners --}

solveWhiteCorners :: CubeWithMoves -> CubeWithMoves
solveWhiteCorners cube = if checkWhiteCornersAndSides cube then cube else solveWhiteCorners $ fixUpWhiteEdges cube

checkWhiteCornersAndSides :: CubeWithMoves -> Bool
checkWhiteCornersAndSides cube = checkWhiteUp cube 
  && checkSide Front front 
  && checkSide Right right 
  && checkSide Left left 
  && checkSide Back back
  where
    (front, left, back, right, up, down) = getSides (fst cube)
    checkSide side colors = colors !! 0 == getTargetSideColor side && colors !! 2 == getTargetSideColor side

checkWhiteUp :: CubeWithMoves -> Bool
checkWhiteUp (cube, moves) = up !! 0 == White && up !! 2 == White && up !! 6 == White && up !! 8 == White
  where up = getSide Up cube

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
  | down !! 0 == White  = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ moveFromDownToSideFront cube
  | down !! 2 == White  = trace ("DEBUG" ++ show cube) $ fixFrontUpEdges $ makeMoveAndNoteWhiteUp Front [D'] cube
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
  | down !! 0 == White  = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ moveFromDownToSideRight cube
  | down !! 2 == White  = trace ("DEBUG" ++ show cube) $ fixRightUpEdges $ makeMoveAndNoteWhiteUp Right [D'] cube
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
  | down !! 0 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ moveFromDownToSideLeft cube
  | down !! 2 == White = trace ("DEBUG" ++ show cube) $ fixLeftUpEdges $ makeMoveAndNoteWhiteUp Left [D'] cube
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
  | down !! 0 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ moveFromDownToSideBack cube
  | down !! 2 == White = trace ("DEBUG" ++ show cube) $ fixBackUpEdges $ makeMoveAndNoteWhiteUp Back [D'] cube
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
    left  = getSide Left (fst cube)
  
moveFromDownToSideRight :: CubeWithMoves -> CubeWithMoves
moveFromDownToSideRight cube
  | front !! 8 == Blue && right !! 6 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) moveFromDownToSideBack $ makeMoveAndNoteWhiteUp Front [D] cube
  where 
    front = getSide Front (fst cube)
    right = getSide Right (fst cube)
  
moveFromDownToSideBack :: CubeWithMoves -> CubeWithMoves
moveFromDownToSideBack cube
  | right !! 8 == Orange && back !! 6 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) moveFromDownToSideLeft $ makeMoveAndNoteWhiteUp Front [D] cube
  where 
    right = getSide Right (fst cube)
    back  = getSide Back (fst cube)

moveFromDownToSideLeft :: CubeWithMoves -> CubeWithMoves
moveFromDownToSideLeft cube
  | back !! 8 == Green && left !! 6 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) moveFromDownToSideFront $ makeMoveAndNoteWhiteUp Front [D] cube
  where 
    back = getSide Back (fst cube)
    left = getSide Left (fst cube)

fixFrontLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixFrontLeftDownEdge cube
  | down !! 0 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixRightLeftDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixRightLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixRightLeftDownEdge cube
  | down !! 2 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixBackLeftDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixBackLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixBackLeftDownEdge cube
  | down !! 8 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixLeftLeftDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixLeftLeftDownEdge :: CubeWithMoves -> CubeWithMoves
fixLeftLeftDownEdge cube
  | down !! 6 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [D, L, D', L'] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixFrontLeftDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixFrontRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixFrontRightDownEdge cube
  | down !! 2 == Red = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Front [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixRightRightDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixRightRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixRightRightDownEdge cube
  | down !! 8 == Blue = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Right [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixBackRightDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixBackRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixBackRightDownEdge cube
  | down !! 6 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Back [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixLeftRightDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

fixLeftRightDownEdge :: CubeWithMoves -> CubeWithMoves
fixLeftRightDownEdge cube
  | down !! 0 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteUp Left [D', R', D, R] cube
  | otherwise = trace ("DEBUG" ++ show cube) fixFrontRightDownEdge $ makeMoveAndNoteWhiteUp Front [D] cube
  where down = getSide Down (fst cube)

{-- Phase 3
mid layer
--}

solveMidLayer :: CubeWithMoves -> CubeWithMoves
solveMidLayer cube = if checkMidLayer cube then cube else solveMidLayer $ fixMidLayer cube

checkMidLayer :: CubeWithMoves -> Bool
checkMidLayer (cube, _) =
     front !! 3 == Red    && front !! 5 == Red
  && left  !! 3 == Green  &&  left !! 5 == Green
  && back  !! 3 == Orange &&  back !! 5 == Orange
  && right !! 3 == Blue   && right !! 5 == Blue
  where
    (front, left, back, right, _, _) = getSides cube


fixMidLayer :: CubeWithMoves -> CubeWithMoves
fixMidLayer cube = fixLeftMidLayer $ fixBackMidLayer $ fixRightMidLayer $ fixFrontMidLayer cube

fixFrontMidLayer :: CubeWithMoves -> CubeWithMoves
fixFrontMidLayer cube
  | front !! 7 /= Yellow && down !! 1 /= Yellow = trace ("DEBUG" ++ show cube) $ fixMidLayerFront cube
  | otherwise = cube
  where 
    front = getSide Front (fst cube)
    down = getSide Down (fst cube)

fixRightMidLayer :: CubeWithMoves -> CubeWithMoves
fixRightMidLayer cube
  | right !! 7 /= Yellow && down !! 5 /= Yellow = trace ("DEBUG" ++ show cube) $ fixMidLayerRight cube
  | otherwise = cube
  where 
    right = getSide Right (fst cube)
    down = getSide Down (fst cube)

fixBackMidLayer :: CubeWithMoves -> CubeWithMoves
fixBackMidLayer cube
  | back !! 7 /= Yellow && down !! 7 /= Yellow = trace ("DEBUG" ++ show cube) $ fixMidLayerBack cube
  | otherwise = cube
  where 
    back = getSide Back (fst cube)
    down = getSide Down (fst cube)

fixLeftMidLayer :: CubeWithMoves -> CubeWithMoves
fixLeftMidLayer cube
  | left !! 7 /= Yellow && down !! 3 /= Yellow = trace ("DEBUG" ++ show cube) $ fixMidLayerLeft cube
  | otherwise = cube
  where 
    left = getSide Left (fst cube)
    down = getSide Down (fst cube)

fixMidLayerFront :: CubeWithMoves -> CubeWithMoves
fixMidLayerFront cube
  | front !! 7 == Red && down !! 1 == Blue  = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteDown Front [U', L', U, L, U, F, U', F'] cube -- Right
  | front !! 7 == Red && down !! 1 == Green = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteDown Front [U, R, U', R', U', F', U, F] cube -- Left
  | otherwise = trace ("DEBUG" ++ show cube) $ fixMidLayerRight $ makeMoveAndNoteWhiteUp Front [D] cube
  where 
    front = getSide Front (fst cube)
    down = getSide Down (fst cube)

fixMidLayerRight :: CubeWithMoves -> CubeWithMoves
fixMidLayerRight cube
  | right !! 7 == Blue && down !! 5 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteDown Right [U', L', U, L, U, F, U', F'] cube
  | right !! 7 == Blue && down !! 5 == Red    = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteDown Right [U, R, U', R', U', F', U, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixMidLayerBack $ makeMoveAndNoteWhiteUp Front [D] cube
  where 
    right = getSide Right (fst cube)
    down = getSide Down (fst cube)

fixMidLayerBack :: CubeWithMoves -> CubeWithMoves
fixMidLayerBack cube
  | back !! 7 == Orange && down !! 7 == Green = trace ("DEBUG" ++ show cube) $  makeMoveAndNoteWhiteDown Back [U', L', U, L, U, F, U', F'] cube
  | back !! 7 == Orange && down !! 7 == Blue  = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteDown Back [U, R, U', R', U', F', U, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixMidLayerLeft $ makeMoveAndNoteWhiteUp Front [D] cube
  where 
    back = getSide Back (fst cube)
    down = getSide Down (fst cube)

fixMidLayerLeft :: CubeWithMoves -> CubeWithMoves
fixMidLayerLeft cube
  | left !! 7 == Green && down !! 3 == Red    = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteDown Left [U', L', U, L, U, F, U', F'] cube
  | left !! 7 == Green && down !! 3 == Orange = trace ("DEBUG" ++ show cube) $ makeMoveAndNoteWhiteDown Left [U, R, U', R', U', F', U, F] cube
  | otherwise = trace ("DEBUG" ++ show cube) $ fixMidLayerFront $ makeMoveAndNoteWhiteUp Front [D] cube
  where 
    left = getSide Left (fst cube)
    down = getSide Down (fst cube)
