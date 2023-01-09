{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Rotations where

import Prelude hiding (Left, Right)
import Types

{-
moveCube :: Move -> Cube -> Cube
moveCube U  cube = moveUp cube
moveCube U' cube = moveUp' cube
moveCube D  cube = moveDown cube
moveCube D' cube = moveDown' cube
moveCube L  cube = moveLeft cube
moveCube L' cube = moveLeft' cube
moveCube R  cube = moveRight cube
moveCube R' cube = moveRight' cube
moveCube B  cube = moveBack cube
moveCube B' cube = moveBack' cube
moveCube _ cube = cube
-}

moveUp :: Types.Cube -> Types.Cube
moveUp cube = rotateTopSideClockwise $ rotateTopRowClockwise cube

moveUp' :: Types.Cube -> Types.Cube
moveUp' cube = rotateTopRowCounterClockwise $ rotateTopSideCounterClockwise cube

moveDown :: Types.Cube -> Types.Cube
moveDown cube = rotateDownRowClockwise $ rotateDownSideClockwise cube

moveDown' :: Types.Cube -> Types.Cube
moveDown' cube = rotateDownRowCounterClockwise $ rotateDownSideCounterClockwise cube

moveLeft :: Types.Cube -> Types.Cube
moveLeft cube = rotateLeftSideToFront $ rotateLeftColumnToFront cube

moveLeft' :: Types.Cube -> Types.Cube
moveLeft' cube = rotateLeftSideToBack $ rotateLeftColumnToBack cube

rotateSideClockwise :: [Types.Color] -> [Types.Color]
rotateSideClockwise side = 
  [
    side !! 6, side !! 3, side !! 0, 
    side !! 7, side !! 4, side !! 1,
    side !! 8, side !! 5, side !! 2
  ]

rotateSideCounterClockwise :: [Types.Color] -> [Types.Color]
rotateSideCounterClockwise side = 
  [
    side !! 2, side !! 5, side !! 8,
    side !! 1, side !! 4, side !! 7,
    side !! 0, side !! 3, side !! 6
  ]

{- TOP TOP TOP TOP TOP TOP TOP TOP -}
rotateTopSideClockwise :: Types.Cube -> Types.Cube
rotateTopSideClockwise cube = 
  [
    (Types.Front, front), 
    (Types.Back, back), 
    (Types.Left, left), 
    (Types.Right, right), 
    (Types.Up, rotateSideClockwise up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateTopSideCounterClockwise :: Types.Cube -> Types.Cube
rotateTopSideCounterClockwise cube = 
  [
    (Types.Front, front), 
    (Types.Back, back), 
    (Types.Left, left), 
    (Types.Right, right), 
    (Types.Up, rotateSideCounterClockwise up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateTopRowClockwise :: Types.Cube -> Types.Cube
rotateTopRowClockwise cube = 
  [
    (Types.Front, replace3 (right !! 0, right !! 1, right !! 2) (0, 1, 2) front), 
    (Types.Left, replace3 (front !! 0, front !! 1, front !! 2) (0, 1, 2) left), 
    (Types.Back, replace3 (left !! 0, left !! 1, left !! 2) (0, 1, 2) back), 
    (Types.Right, replace3 (back !! 0, back !! 1, back !! 2) (0, 1, 2) right), 
    (Types.Up, up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateTopRowCounterClockwise :: Types.Cube -> Types.Cube
rotateTopRowCounterClockwise cube = 
  [
    (Types.Front, replace3 (left !! 0, left !! 1, left !! 2) (0, 1, 2) front), 
    (Types.Right, replace3 (front !! 0, front !! 1, front !! 2) (0, 1, 2) right), 
    (Types.Back, replace3 (right !! 0, right !! 1, right !! 2) (0, 1, 2) back), 
    (Types.Left, replace3 (back !! 0, back !! 1, back !! 2) (0, 1, 2) left), 
    (Types.Up, up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

{- DOWN DOWN DOWN DOWN DOWN DOWN DOWN DOWN -}
rotateDownRowClockwise :: Types.Cube -> Types.Cube
rotateDownRowClockwise cube = 
  [
    (Types.Front, replace3 (right !! 6, right !! 7, right !! 8) (6, 7, 8) front), 
    (Types.Left, replace3 (front !! 6, front !! 7, front !! 8) (6, 7, 8) left), 
    (Types.Back, replace3 (left !! 6, left !! 7, left !! 8) (6, 7, 8) back), 
    (Types.Right, replace3 (back !! 6, back !! 7, back !! 8) (6, 7, 8) right), 
    (Types.Up, up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateDownRowCounterClockwise :: Types.Cube -> Types.Cube
rotateDownRowCounterClockwise cube = 
  [
    (Types.Front, replace3 (left !! 6, left !! 7, left !! 8) (6, 7, 8) front), 
    (Types.Right, replace3 (front !! 6, front !! 7, front !! 8) (6, 7, 8) right), 
    (Types.Back, replace3 (right !! 6, right !! 7, right !! 8) (6, 7, 8) back), 
    (Types.Left, replace3 (back !! 6, back !! 7, back !! 8) (6, 7, 8) left), 
    (Types.Up, up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateDownSideClockwise :: Types.Cube -> Types.Cube
rotateDownSideClockwise cube = 
  [
    (Types.Front, front), 
    (Types.Back, back), 
    (Types.Left, left), 
    (Types.Right, right), 
    (Types.Up, up), 
    (Types.Down, rotateSideClockwise down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateDownSideCounterClockwise :: Types.Cube -> Types.Cube
rotateDownSideCounterClockwise cube = 
  [
    (Types.Front, front), 
    (Types.Back, back), 
    (Types.Left, left), 
    (Types.Right, right), 
    (Types.Up, up), 
    (Types.Down, rotateSideCounterClockwise down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube


{- LEFT LEFT LEFT LEFT LEFT LEFT -}

rotateLeftColumnToFront :: Types.Cube -> Types.Cube
rotateLeftColumnToFront cube = 
  [
    (Types.Front, replace3 (up !! 0, up !! 3, up !! 6) (0, 3, 6) front), 
    (Types.Right, right),
    (Types.Back, replace3 (down !! 6, down !! 3, down !! 0) (2, 5, 8) back), 
    (Types.Left, left),
    (Types.Up, replace3 (back !! 8, back !! 5, back !! 2) (0, 3, 6) up), 
    (Types.Down, replace3 (front !! 0, front !! 3, front !! 6) (0, 3, 6) down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube  

rotateLeftColumnToBack :: Types.Cube -> Types.Cube
rotateLeftColumnToBack cube = 
  [
    (Types.Front, replace3 (down !! 0, down !! 3, down !! 6) (0, 3, 6) front), 
    (Types.Right, right),
    (Types.Back, replace3 (up !! 6, up !! 3, up !! 0) (2, 5, 8) back), 
    (Types.Left, left),
    (Types.Up, replace3 (front !! 0, front !! 3, front !! 6) (0, 3, 6) front), 
    (Types.Down, replace3 (back !! 8, back !! 5, back !! 2) (0, 3, 6) down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateLeftSideToFront :: Types.Cube -> Types.Cube
rotateLeftSideToFront cube = 
  [
    (Types.Front, front), 
    (Types.Back, back), 
    (Types.Left, rotateSideClockwise left), 
    (Types.Right, right), 
    (Types.Up, up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

rotateLeftSideToBack :: Types.Cube -> Types.Cube
rotateLeftSideToBack cube = 
  [
    (Types.Front, front), 
    (Types.Back, back), 
    (Types.Left, rotateSideCounterClockwise left), 
    (Types.Right, right), 
    (Types.Up, up), 
    (Types.Down, down)
  ]
  where
    (front, left, back, right, up, down) = Types.getSides cube

replace3 :: (a, a, a) -> (Int, Int, Int) -> [a] -> [a]
replace3 (v1, v2, v3) (t1, t2, t3) target = replace v3 t3 (replace v2 t2 (replace v1 t1 target))

replace :: a -> Int -> [a] -> [a]
replace _ _ [] = []
replace value n (x:xs) = if n == 0 then value : xs else x : replace value (n - 1) xs
