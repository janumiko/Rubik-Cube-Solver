{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
import Prelude hiding (Left, Right)

{- Types -}

data Color = White | Yellow | Orange | Green | Red | Blue deriving (Eq, Show) 

data Side = Front | Up | Left | Right | Back | Down deriving (Eq, Show)

type Face = (Side, [Color])

type Cube = [Face]

data Move = U | U' | D | D' | L | L' | R | R' | F | F' | B | B' deriving Show

{- Getters -}

getSolvedCube :: Cube
getSolvedCube =
  [ (Down, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
    (Up, [White, White, White, White, White, White, White, White, White]),
    (Left, [Green, Green, Green, Green, Green, Green, Green, Green, Green]),
    (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue]),
    (Front, [Red, Red, Red, Red, Red, Red, Red, Red, Red]),
    (Back, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange])
  ]

getTargetSideColor :: Side -> Color
getTargetSideColor Up = Yellow
getTargetSideColor Down = White
getTargetSideColor Left = Green
getTargetSideColor Right = Blue
getTargetSideColor Front = Orange
getTargetSideColor Back = Red

getSide :: Side -> Cube -> [Color]
getSide side cube = concat [snd face | face <- cube, side == fst face]

getSides :: Cube -> ([Color], [Color], [Color], [Color], [Color], [Color])
getSides cube = (getSide Front cube, getSide Left cube, getSide Back cube, getSide Right cube, getSide Up cube, getSide Down cube)

{- MOVES -}

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

moveUp :: Cube -> Cube
moveUp cube = rotateTopSideClockwise $ rotateTopRowClockwise cube

moveUp' :: Cube -> Cube
moveUp' cube = rotateTopRowCounterClockwise $ rotateTopSideCounterClockwise cube

moveDown :: Cube -> Cube
moveDown cube = rotateDownRowClockwise $ rotateDownSideClockwise cube

moveDown' :: Cube -> Cube
moveDown' cube = rotateDownRowCounterClockwise $ rotateDownSideCounterClockwise cube

rotateSideClockwise :: [Color] -> [Color]
rotateSideClockwise side = 
  [
    side !! 6, side !! 3, side !! 0, 
    side !! 7, side !! 4, side !! 1,
    side !! 8, side !! 5, side !! 2
  ]

rotateSideCounterClockwise :: [Color] -> [Color]
rotateSideCounterClockwise side = 
  [
    side !! 2, side !! 5, side !! 8,
    side !! 1, side !! 4, side !! 7,
    side !! 0, side !! 3, side !! 6
  ]

{- TOP TOP TOP TOP TOP TOP TOP TOP -}
rotateTopSideClockwise :: Cube -> Cube
rotateTopSideClockwise cube = 
  [
    (Front, front), 
    (Back, back), 
    (Left, left), 
    (Right, right), 
    (Up, rotateSideClockwise up), 
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateTopSideCounterClockwise :: Cube -> Cube
rotateTopSideCounterClockwise cube = 
  [
    (Front, front), 
    (Back, back), 
    (Left, left), 
    (Right, right), 
    (Up, rotateSideCounterClockwise up), 
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateTopRowClockwise :: Cube -> Cube
rotateTopRowClockwise cube = 
  [
    (Front, replace3 (right !! 0, right !! 1, right !! 2) (0, 1, 2) front), 
    (Left, replace3 (front !! 0, front !! 1, front !! 2) (0, 1, 2) left), 
    (Back, replace3 (left !! 0, left !! 1, left !! 2) (0, 1, 2) back), 
    (Right, replace3 (back !! 0, back !! 1, back !! 2) (0, 1, 2) right), 
    (Up, up), 
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateTopRowCounterClockwise :: Cube -> Cube
rotateTopRowCounterClockwise cube = 
  [
    (Front, replace3 (left !! 0, left !! 1, left !! 2) (0, 1, 2) front), 
    (Right, replace3 (front !! 0, front !! 1, front !! 2) (0, 1, 2) right), 
    (Back, replace3 (right !! 0, right !! 1, right !! 2) (0, 1, 2) back), 
    (Left, replace3 (back !! 0, back !! 1, back !! 2) (0, 1, 2) left), 
    (Up, up), 
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

{- DOWN DOWN DOWN DOWN DOWN DOWN DOWN DOWN -}
rotateDownRowClockwise :: Cube -> Cube
rotateDownRowClockwise cube = 
  [
    (Front, replace3 (right !! 6, right !! 7, right !! 8) (6, 7, 8) front), 
    (Left, replace3 (front !! 6, front !! 7, front !! 8) (6, 7, 8) left), 
    (Back, replace3 (left !! 6, left !! 7, left !! 8) (6, 7, 8) back), 
    (Right, replace3 (back !! 6, back !! 7, back !! 8) (6, 7, 8) right), 
    (Up, up), 
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateDownRowCounterClockwise :: Cube -> Cube
rotateDownRowCounterClockwise cube = 
  [
    (Front, replace3 (left !! 6, left !! 7, left !! 8) (6, 7, 8) front), 
    (Right, replace3 (front !! 6, front !! 7, front !! 8) (6, 7, 8) right), 
    (Back, replace3 (right !! 6, right !! 7, right !! 8) (6, 7, 8) back), 
    (Left, replace3 (back !! 6, back !! 7, back !! 8) (6, 7, 8) left), 
    (Up, up), 
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateDownSideClockwise :: Cube -> Cube
rotateDownSideClockwise cube = 
  [
    (Front, front), 
    (Back, back), 
    (Left, left), 
    (Right, right), 
    (Up, up), 
    (Down, rotateSideClockwise down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateDownSideCounterClockwise :: Cube -> Cube
rotateDownSideCounterClockwise cube = 
  [
    (Front, front), 
    (Back, back), 
    (Left, left), 
    (Right, right), 
    (Up, up), 
    (Down, rotateSideCounterClockwise down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube


replace3 :: (a, a, a) -> (Int, Int, Int) -> [a] -> [a]
replace3 (v1, v2, v3) (t1, t2, t3) target = replace v3 t3 (replace v2 t2 (replace v1 t1 target))

replace :: a -> Int -> [a] -> [a]
replace _ _ [] = []
replace value n (x:xs) = if n == 0 then value : xs else x : replace value (n - 1) xs
