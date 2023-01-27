module Main where

import Rotations
import Solving
import System.Environment
import Types
import Utils
import Prelude hiding (Left, Right)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  content <- readFile fileName
  let fileLines = lines content
  let cube = readCube fileLines []
  print (snd $ solveCube (makeMoves [F, F, D', R', R, L, D, F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', B, B, L, R', D, D, R, L', B, F, F, D', U', L, D, U', B', R, D, U] cube))

main2 :: IO ()
main2 = print (solveCube (makeMoves [R, L, D, F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', B, B, L, R', D, D, R, L', B, F, F, D', U', L, D, U', B', R, D, U] getSolvedCube))

readCube :: [String] -> Cube -> Cube
readCube [] cube = cube
readCube (x : xs) cube =
  let w = words x
   in let side = head w
       in let currentCube = ((translateSide side, map translateColor (tail w)) :: Face) : cube
           in readCube xs currentCube

translateSide :: String -> Side
translateSide "Down" = Down
translateSide "Up" = Up
translateSide "Left" = Left
translateSide "Right" = Right
translateSide "Front" = Front
translateSide "Back" = Back
translateSide _ = Up

translateColor :: String -> Color
translateColor "Blue" = Blue
translateColor "Red" = Red
translateColor "Yellow" = Yellow
translateColor "White" = White
translateColor "Orange" = Orange
translateColor "Green" = Green
translateColor _ = White
