module Main where

import Rotations
import Solving
import Types
import Utils
import Prelude hiding (Left, Right)

randomCube :: [(Side, [Color])]
randomCube =
  [ (Down, [Yellow, Red, White, White, Yellow, Yellow, Yellow, Yellow, White]),
    (Up, [White, Yellow, Yellow, Yellow, White, Blue, White, White, Yellow]),
    (Left, [Blue, Blue, Green, White, Green, Green, Green, Red, Red]),
    (Right, [Green, Orange, Orange, Green, Blue, Green, Orange, Red, Orange]),
    (Front, [Red, Blue, Red, White, Red, Orange, Blue, Blue, Green]),
    (Back, [Blue, Green, Red, Red, Orange, Orange, Blue, Orange, Orange])
  ]

main :: IO ()
main = print (solveCube (makeMoves [R,L,D,F,D,D,U,L',R,R,L,D,D,U',B,B,L,R',D,D,R,L',B,F,F,R,L,F,B',B,B,L,R',D,D,R,L',B,F,F,D',U',L,D,U',B',R,D,U] getSolvedCube))
