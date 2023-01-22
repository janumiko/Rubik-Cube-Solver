module Types where

import Prelude hiding (Left, Right)

{- Types -}

data Color = White | Yellow | Orange | Green | Red | Blue deriving (Eq, Show)

data Side = Front | Up | Left | Right | Back | Down deriving (Eq, Show)

type Face = (Side, [Color])

type Cube = [Face]

data Move = U | U' | D | D' | L | L' | R | R' | F | F' | B | B' deriving (Eq, Show)
