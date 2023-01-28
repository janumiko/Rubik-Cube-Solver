module Rotations where

import Types
import Utils
import Prelude hiding (Left, Right)

makeMoveAndNoteWhiteUp :: Side -> [Move] -> CubeWithMoves -> CubeWithMoves
makeMoveAndNoteWhiteUp side moves (cube, past_moves) =
  (makeMoves translated_moves cube, past_moves ++ translated_moves)
  where
    translated_moves = translateMovesWhiteUp side moves

makeMoveAndNoteWhiteDown :: Side -> [Move] -> CubeWithMoves -> CubeWithMoves
makeMoveAndNoteWhiteDown side moves (cube, past_moves) =
  (makeMoves translated_moves cube, past_moves ++ translated_moves)
  where
    translated_moves = translateMovesWhiteDown side moves

makeMoves :: [Move] -> Cube -> Cube
makeMoves moves cube = foldl (flip moveCube) cube moves

moveCube :: Move -> Cube -> Cube
moveCube U cube = moveUp cube
moveCube U' cube = moveUp' cube
moveCube D cube = moveDown cube
moveCube D' cube = moveDown' cube
moveCube L cube = moveLeft cube
moveCube L' cube = moveLeft' cube
moveCube R cube = moveRight cube
moveCube R' cube = moveRight' cube
moveCube B cube = moveBack cube
moveCube B' cube = moveBack' cube
moveCube F cube = moveFront cube
moveCube F' cube = moveFront' cube

moveUp :: Cube -> Cube
moveUp cube = rotateTopSideClockwise $ rotateTopRowClockwise cube

moveUp' :: Cube -> Cube
moveUp' cube = rotateTopRowCounterClockwise $ rotateTopSideCounterClockwise cube

moveDown' :: Cube -> Cube
moveDown' cube = rotateDownRowClockwise $ rotateDownSideCounterClockwise cube

moveDown :: Cube -> Cube
moveDown cube = rotateDownRowCounterClockwise $ rotateDownSideClockwise cube

moveLeft :: Cube -> Cube
moveLeft cube = rotateLeftSideToFront $ rotateLeftColumnToFront cube

moveLeft' :: Cube -> Cube
moveLeft' cube = rotateLeftSideToBack $ rotateLeftColumnToBack cube

moveRight :: Cube -> Cube
moveRight cube = rotateRightSideToBack $ rotateRightColumnToBack cube

moveRight' :: Cube -> Cube
moveRight' cube = rotateRightSideToFront $ rotateRightColumnToFront cube

moveFront :: Cube -> Cube
moveFront cube = rotateFrontSideClockwise $ rotateFrontRowsClockwise cube

moveFront' :: Cube -> Cube
moveFront' cube = rotateFrontSideCounterClockwise $ rotateFrontRowsCounterClockwise cube

moveBack :: Cube -> Cube
moveBack cube = rotateBackSideClockwise $ rotateBackRowsClockwise cube

moveBack' :: Cube -> Cube
moveBack' cube = rotateBackSideCounterClockwise $ rotateBackRowsCounterClockwise cube

rotateSideClockwise :: [Color] -> [Color]
rotateSideClockwise side =
  [ side !! 6,
    side !! 3,
    side !! 0,
    side !! 7,
    side !! 4,
    side !! 1,
    side !! 8,
    side !! 5,
    side !! 2
  ]

rotateSideCounterClockwise :: [Color] -> [Color]
rotateSideCounterClockwise side =
  [ side !! 2,
    side !! 5,
    side !! 8,
    side !! 1,
    side !! 4,
    side !! 7,
    side !! 0,
    side !! 3,
    side !! 6
  ]

{- TOP TOP TOP TOP TOP TOP TOP TOP -}
rotateTopSideClockwise :: Cube -> Cube
rotateTopSideClockwise cube =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, rotateSideClockwise up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateTopSideCounterClockwise :: Cube -> Cube
rotateTopSideCounterClockwise cube =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, rotateSideCounterClockwise up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateTopRowClockwise :: Cube -> Cube
rotateTopRowClockwise cube =
  [ (Front, replace3 (right !! 0, right !! 1, right !! 2) (0, 1, 2) front),
    (Right, replace3 (back !! 0, back !! 1, back !! 2) (0, 1, 2) right),
    (Back, replace3 (left !! 0, left !! 1, left !! 2) (0, 1, 2) back),
    (Left, replace3 (front !! 0, front !! 1, front !! 2) (0, 1, 2) left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateTopRowCounterClockwise :: Cube -> Cube
rotateTopRowCounterClockwise cube =
  [ (Front, replace3 (left !! 0, left !! 1, left !! 2) (0, 1, 2) front),
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
  [ (Front, replace3 (right !! 6, right !! 7, right !! 8) (6, 7, 8) front),
    (Right, replace3 (back !! 6, back !! 7, back !! 8) (6, 7, 8) right),
    (Back, replace3 (left !! 6, left !! 7, left !! 8) (6, 7, 8) back),
    (Left, replace3 (front !! 6, front !! 7, front !! 8) (6, 7, 8) left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateDownRowCounterClockwise :: Cube -> Cube
rotateDownRowCounterClockwise cube =
  [ (Front, replace3 (left !! 6, left !! 7, left !! 8) (6, 7, 8) front),
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
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, rotateSideClockwise down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateDownSideCounterClockwise :: Cube -> Cube
rotateDownSideCounterClockwise cube =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, rotateSideCounterClockwise down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

{- LEFT LEFT LEFT LEFT LEFT LEFT -}

rotateLeftColumnToFront :: Cube -> Cube
rotateLeftColumnToFront cube =
  [ (Front, replace3 (up !! 0, up !! 3, up !! 6) (0, 3, 6) front),
    (Right, right),
    (Back, replace3 (down !! 6, down !! 3, down !! 0) (2, 5, 8) back),
    (Left, left),
    (Up, replace3 (back !! 8, back !! 5, back !! 2) (0, 3, 6) up),
    (Down, replace3 (front !! 0, front !! 3, front !! 6) (0, 3, 6) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateLeftColumnToBack :: Cube -> Cube
rotateLeftColumnToBack cube =
  [ (Front, replace3 (down !! 0, down !! 3, down !! 6) (0, 3, 6) front),
    (Right, right),
    (Back, replace3 (up !! 6, up !! 3, up !! 0) (2, 5, 8) back),
    (Left, left),
    (Up, replace3 (front !! 0, front !! 3, front !! 6) (0, 3, 6) up),
    (Down, replace3 (back !! 8, back !! 5, back !! 2) (0, 3, 6) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateLeftSideToFront :: Cube -> Cube
rotateLeftSideToFront cube =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, rotateSideClockwise left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateLeftSideToBack :: Cube -> Cube
rotateLeftSideToBack cube =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, rotateSideCounterClockwise left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

{- RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT -}

rotateRightColumnToFront :: Cube -> Cube
rotateRightColumnToFront cube =
  [ (Front, replace3 (up !! 2, up !! 5, up !! 8) (2, 5, 8) front),
    (Right, right),
    (Back, replace3 (down !! 8, down !! 5, down !! 2) (0, 3, 6) back),
    (Left, left),
    (Up, replace3 (back !! 6, back !! 3, back !! 0) (2, 5, 8) up),
    (Down, replace3 (front !! 2, front !! 5, front !! 8) (2, 5, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateRightColumnToBack :: Cube -> Cube
rotateRightColumnToBack cube =
  [ (Front, replace3 (down !! 2, down !! 5, down !! 8) (2, 5, 8) front),
    (Right, right),
    (Back, replace3 (up !! 8, up !! 5, up !! 2) (0, 3, 6) back),
    (Left, left),
    (Up, replace3 (front !! 2, front !! 5, front !! 8) (2, 5, 8) up),
    (Down, replace3 (back !! 6, back !! 3, back !! 0) (2, 5, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateRightSideToFront :: Cube -> Cube
rotateRightSideToFront cube =
  [ (Front, front),
    (Right, rotateSideCounterClockwise right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateRightSideToBack :: Cube -> Cube
rotateRightSideToBack cube =
  [ (Front, front),
    (Right, rotateSideClockwise right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

{- FRONT FRONT FRONT FRONT FRONT FRONT -}

rotateFrontRowsClockwise :: Cube -> Cube
rotateFrontRowsClockwise cube =
  [ (Front, front),
    (Right, replace3 (up !! 6, up !! 7, up !! 8) (0, 3, 6) right),
    (Back, back),
    (Left, replace3 (down !! 0, down !! 1, down !! 2) (2, 5, 8) left),
    (Up, replace3 (left !! 2, left !! 5, left !! 8) (8, 7, 6) up),
    (Down, replace3 (right !! 6, right !! 3, right !! 0) (0, 1, 2) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateFrontRowsCounterClockwise :: Cube -> Cube
rotateFrontRowsCounterClockwise cube =
  [ (Front, front),
    (Right, replace3 (down !! 2, down !! 1, down !! 0) (0, 3, 6) right),
    (Back, back),
    (Left, replace3 (up !! 8, up !! 7, up !! 6) (2, 5, 8) left),
    (Up, replace3 (right !! 0, right !! 3, right !! 6) (6, 7, 8) up),
    (Down, replace3 (left !! 2, left !! 5, left !! 8) (0, 1, 2) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateFrontSideClockwise :: Cube -> Cube
rotateFrontSideClockwise cube =
  [ (Front, rotateSideClockwise front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateFrontSideCounterClockwise :: Cube -> Cube
rotateFrontSideCounterClockwise cube =
  [ (Front, rotateSideCounterClockwise front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

{- BACK BACK BACK BACK BACK BACK -}

rotateBackRowsClockwise :: Cube -> Cube
rotateBackRowsClockwise cube =
  [ (Front, front),
    (Right, replace3 (down !! 8, down !! 7, down !! 6) (2, 5, 8) right),
    (Back, back),
    (Left, replace3 (up !! 2, up !! 1, up !! 0) (0, 3, 6) left),
    (Up, replace3 (right !! 2, right !! 5, right !! 8) (0, 1, 2) up),
    (Down, replace3 (left !! 0, left !! 3, left !! 6) (6, 7, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateBackRowsCounterClockwise :: Cube -> Cube
rotateBackRowsCounterClockwise cube =
  [ (Front, front),
    (Right, replace3 (up !! 0, up !! 1, up !! 2) (2, 5, 8) right),
    (Back, back),
    (Left, replace3 (down !! 6, down !! 7, down !! 8) (0, 3, 6) left),
    (Up, replace3 (left !! 6, left !! 3, left !! 0) (0, 1, 2) up),
    (Down, replace3 (right !! 8, right !! 5, right !! 2) (6, 7, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateBackSideClockwise :: Cube -> Cube
rotateBackSideClockwise cube =
  [ (Front, front),
    (Right, right),
    (Back, rotateSideClockwise back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube

rotateBackSideCounterClockwise :: Cube -> Cube
rotateBackSideCounterClockwise cube =
  [ (Front, front),
    (Right, right),
    (Back, rotateSideCounterClockwise back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides cube