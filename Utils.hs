module Utils where

import Prelude hiding (Left, Right)
import Types

replace3 :: (a, a, a) -> (Int, Int, Int) -> [a] -> [a]
replace3 (v1, v2, v3) (t1, t2, t3) target = replace v3 t3 (replace v2 t2 (replace v1 t1 target))

replace :: a -> Int -> [a] -> [a]
replace _ _ [] = []
replace value n (x:xs) = if n == 0 then value : xs else x : replace value (n - 1) xs

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