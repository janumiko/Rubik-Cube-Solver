module Types where

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