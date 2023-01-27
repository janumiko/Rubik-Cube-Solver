import Rotations
import Solving
import Types
import Utils
import Prelude hiding (Left, Right)

testCube =
  [ (Front, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange]),
    (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue]),
    (Back, [Red, Red, Red, Red, Red, Red, Red, Red, Red]),
    (Left, [Green, Green, Green, Green, Green, Green, Green, Green, Green]),
    (Up, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
    (Down, [White, White, White, White, White, White, White, White, White])
  ] ::
    Cube

testMakeMoves :: Bool
testMakeMoves = makeMoves [F, F', D, D', U, U', L, L', R, R', B, B'] testCube == testCube

testUpUp :: Bool
testUpUp = moveUp' (moveUp testCube) == testCube && moveUp (moveUp' testCube) == testCube

testDownDown :: Bool
testDownDown = moveDown (moveDown' testCube) == testCube && moveDown' (moveDown testCube) == testCube

testLeftLeft :: Bool
testLeftLeft = moveLeft (moveLeft' testCube) == testCube && moveLeft' (moveLeft testCube) == testCube

testRightRight :: Bool
testRightRight = moveRight (moveRight' testCube) == testCube && moveRight' (moveRight testCube) == testCube

testFrontFront :: Bool
testFrontFront = moveFront (moveFront' testCube) == testCube && moveFront' (moveFront testCube) == testCube

testBackBack :: Bool
testBackBack = moveBack (moveBack' testCube) == testCube && moveBack' (moveBack testCube) == testCube

testUp :: Bool
testUp =
  moveUp testCube
    == ( [ (Front, [Blue, Blue, Blue, Orange, Orange, Orange, Orange, Orange, Orange]),
           (Right, [Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue]),
           (Back, [Green, Green, Green, Red, Red, Red, Red, Red, Red]),
           (Left, [Orange, Orange, Orange, Green, Green, Green, Green, Green, Green]),
           (Up, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
           (Down, [White, White, White, White, White, White, White, White, White])
         ] ::
           Cube
       )

testUp' :: Bool
testUp' =
  moveUp' testCube
    == ( [ (Front, [Green, Green, Green, Orange, Orange, Orange, Orange, Orange, Orange]),
           (Right, [Orange, Orange, Orange, Blue, Blue, Blue, Blue, Blue, Blue]),
           (Back, [Blue, Blue, Blue, Red, Red, Red, Red, Red, Red]),
           (Left, [Red, Red, Red, Green, Green, Green, Green, Green, Green]),
           (Up, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
           (Down, [White, White, White, White, White, White, White, White, White])
         ] ::
           Cube
       )

testDown :: Bool
testDown =
  moveDown testCube
    == ( [ (Front, [Orange, Orange, Orange, Orange, Orange, Orange, Green, Green, Green]),
           (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Orange, Orange, Orange]),
           (Back, [Red, Red, Red, Red, Red, Red, Blue, Blue, Blue]),
           (Left, [Green, Green, Green, Green, Green, Green, Red, Red, Red]),
           (Up, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
           (Down, [White, White, White, White, White, White, White, White, White])
         ] ::
           Cube
       )

testDown' :: Bool
testDown' =
  moveDown' testCube
    == ( [ (Front, [Orange, Orange, Orange, Orange, Orange, Orange, Blue, Blue, Blue]),
           (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Red, Red, Red]),
           (Back, [Red, Red, Red, Red, Red, Red, Green, Green, Green]),
           (Left, [Green, Green, Green, Green, Green, Green, Orange, Orange, Orange]),
           (Up, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
           (Down, [White, White, White, White, White, White, White, White, White])
         ] ::
           Cube
       )

testLeft :: Bool
testLeft =
  moveLeft testCube
    == ( [ (Front, [Yellow, Orange, Orange, Yellow, Orange, Orange, Yellow, Orange, Orange]),
           (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue]),
           (Back, [Red, Red, White, Red, Red, White, Red, Red, White]),
           (Left, [Green, Green, Green, Green, Green, Green, Green, Green, Green]),
           (Up, [Red, Yellow, Yellow, Red, Yellow, Yellow, Red, Yellow, Yellow]),
           (Down, [Orange, White, White, Orange, White, White, Orange, White, White])
         ] ::
           Cube
       )

testLeft' :: Bool
testLeft' =
  moveLeft' testCube
    == ( [ (Front, [White, Orange, Orange, White, Orange, Orange, White, Orange, Orange]),
           (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue]),
           (Back, [Red, Red, Yellow, Red, Red, Yellow, Red, Red, Yellow]),
           (Left, [Green, Green, Green, Green, Green, Green, Green, Green, Green]),
           (Up, [Orange, Yellow, Yellow, Orange, Yellow, Yellow, Orange, Yellow, Yellow]),
           (Down, [Red, White, White, Red, White, White, Red, White, White])
         ] ::
           Cube
       )

testRight :: Bool
testRight =
  moveRight testCube
    == ( [ (Front, [Orange, Orange, White, Orange, Orange, White, Orange, Orange, White]),
           (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue]),
           (Back, [Yellow, Red, Red, Yellow, Red, Red, Yellow, Red, Red]),
           (Left, [Green, Green, Green, Green, Green, Green, Green, Green, Green]),
           (Up, [Yellow, Yellow, Orange, Yellow, Yellow, Orange, Yellow, Yellow, Orange]),
           (Down, [White, White, Red, White, White, Red, White, White, Red])
         ] ::
           Cube
       )

testRight' :: Bool
testRight' =
  moveRight' testCube
    == ( [ (Front, [Orange, Orange, Yellow, Orange, Orange, Yellow, Orange, Orange, Yellow]),
           (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue]),
           (Back, [White, Red, Red, White, Red, Red, White, Red, Red]),
           (Left, [Green, Green, Green, Green, Green, Green, Green, Green, Green]),
           (Up, [Yellow, Yellow, Red, Yellow, Yellow, Red, Yellow, Yellow, Red]),
           (Down, [White, White, Orange, White, White, Orange, White, White, Orange])
         ] ::
           Cube
       )

testFront :: Bool
testFront =
  moveFront testCube
    == ( [ (Front, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange]),
           (Right, [Yellow, Blue, Blue, Yellow, Blue, Blue, Yellow, Blue, Blue]),
           (Back, [Red, Red, Red, Red, Red, Red, Red, Red, Red]),
           (Left, [Green, Green, White, Green, Green, White, Green, Green, White]),
           (Up, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Green, Green, Green]),
           (Down, [Blue, Blue, Blue, White, White, White, White, White, White])
         ] ::
           Cube
       )

testFront' :: Bool
testFront' =
  moveFront' testCube
    == ( [ (Front, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange]),
           (Right, [White, Blue, Blue, White, Blue, Blue, White, Blue, Blue]),
           (Back, [Red, Red, Red, Red, Red, Red, Red, Red, Red]),
           (Left, [Green, Green, Yellow, Green, Green, Yellow, Green, Green, Yellow]),
           (Up, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Blue, Blue, Blue]),
           (Down, [Green, Green, Green, White, White, White, White, White, White])
         ] ::
           Cube
       )

testBack :: Bool
testBack =
  moveBack testCube
    == ( [ (Front, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange]),
           (Right, [Blue, Blue, White, Blue, Blue, White, Blue, Blue, White]),
           (Back, [Red, Red, Red, Red, Red, Red, Red, Red, Red]),
           (Left, [Yellow, Green, Green, Yellow, Green, Green, Yellow, Green, Green]),
           (Up, [Blue, Blue, Blue, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
           (Down, [White, White, White, White, White, White, Green, Green, Green])
         ] ::
           Cube
       )

testBack' :: Bool
testBack' =
  moveBack' testCube
    == ( [ (Front, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange]),
           (Right, [Blue, Blue, Yellow, Blue, Blue, Yellow, Blue, Blue, Yellow]),
           (Back, [Red, Red, Red, Red, Red, Red, Red, Red, Red]),
           (Left, [White, Green, Green, White, Green, Green, White, Green, Green]),
           (Up, [Green, Green, Green, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]),
           (Down, [White, White, White, White, White, White, Blue, Blue, Blue])
         ] ::
           Cube
       )

debugCube =
  [ (Front, [Red, Red, Yellow, Red, Red, Yellow, Blue, Blue, Red]),
    (Right, [Blue, Blue, White, Blue, Blue, White, White, White, White]),
    (Back, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Green, Green]),
    (Left, [Yellow, Green, Green, Yellow, Green, Green, Red, Red, Yellow]),
    (Up, [Green, Green, Green, White, White, Red, White, White, Red]),
    (Down, [Blue, Yellow, Yellow, Blue, Yellow, Yellow, Blue, Orange, Orange])
  ]

testSolveCompletes :: Cube -> Bool
testSolveCompletes cube = not (null (fst (solveCube cube)))

testAll :: Bool
testAll =
  testBack && testBack'
    && testDown
    && testDown'
    && testFront
    && testFront'
    && testLeft
    && testLeft'
    && testRight
    && testRight'
    && testUp
    && testUp'
    && testDownDown
    && testLeftLeft
    && testRightRight
    && testUpUp
    && testFrontFront
    && testBackBack
    && testMakeMoves

testSolveCompletesAll :: Bool
testSolveCompletesAll =
  testSolveCompletes getSolvedCube
    && testSolveCompletes (moveLeft getSolvedCube)
    && testSolveCompletes (moveLeft (moveBack' (moveDown (moveUp' (moveRight (moveLeft getSolvedCube))))))
    && testSolveCompletes (moveLeft' (moveRight (moveUp' (moveDown' (moveLeft (moveBack' (moveDown (moveUp' (moveRight (moveLeft getSolvedCube))))))))))
    && testSolveCompletes (moveLeft' (moveRight (moveUp' (moveDown' (moveLeft (moveBack' (moveDown (moveUp' (moveRight (moveLeft getSolvedCube))))))))))
    && testSolveCompletes (makeMoves [F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D', U', L, D, U', B', R, D, U] getSolvedCube)
    && testSolveCompletes (makeMoves [D, D, R, L', B, F, F, R, L, F, B', F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', D', U', L, D, U', B', R, D, U] getSolvedCube)
    && testSolveCompletes (makeMoves [R, L, D, F, D, D, U, L', R, D, F, D, D, U, L', R, R, L, D, D, U', R, L, F, B', D', U', L, D, U', B', R, D, U, L, D, U', B', R, D, U] getSolvedCube)
    && testSolveCompletes (makeMoves [D, U', B, B, F, D, D, U, L', R, R, L, D, L, R', D, D, R, L', B, F, F, R, L, F, B', D, D, U, L', R, R, L, D, D, D', U', L, D, U', B', R, D, U] getSolvedCube)
    && testSolveCompletes (makeMoves [R, L, D, F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', B, B, L, R', D, D, R, L', B, F, F, D', U', L, D, U', B', R, D, U] getSolvedCube)
    && testSolveCompletes (makeMoves [F, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', D', U'] getSolvedCube)
    && testSolveCompletes (makeMoves [U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', D', U', L, D, U', B', R, D, U] getSolvedCube)

testE2E :: Bool
testE2E =
  fst (solveCube getSolvedCube) == getSolvedCube
    && fst (solveCube (moveLeft getSolvedCube)) == getSolvedCube
    && fst (solveCube (moveLeft (moveBack' (moveDown (moveUp' (moveRight (moveLeft getSolvedCube))))))) == getSolvedCube
    && fst (solveCube (moveLeft' (moveRight (moveUp' (moveDown' (moveLeft (moveBack' (moveDown (moveUp' (moveRight (moveLeft getSolvedCube))))))))))) == getSolvedCube
    && fst (solveCube (moveLeft' (moveRight (moveUp' (moveDown' (moveLeft (moveBack' (moveDown (moveUp' (moveRight (moveLeft getSolvedCube))))))))))) == getSolvedCube
    && fst (solveCube (makeMoves [F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D', U', L, D, U', B', R, D, U] getSolvedCube)) == getSolvedCube
    && fst (solveCube (makeMoves [D, D, R, L', B, F, F, R, L, F, B', F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', D', U', L, D, U', B', R, D, U] getSolvedCube)) == getSolvedCube
    && fst (solveCube (makeMoves [R, L, D, F, D, D, U, L', R, D, F, D, D, U, L', R, R, L, D, D, U', R, L, F, B', D', U', L, D, U', B', R, D, U, L, D, U', B', R, D, U] getSolvedCube)) == getSolvedCube
    && fst (solveCube (makeMoves [D, U', B, B, F, D, D, U, L', R, R, L, D, L, R', D, D, R, L', B, F, F, R, L, F, B', D, D, U, L', R, R, L, D, D, D', U', L, D, U', B', R, D, U] getSolvedCube)) == getSolvedCube
    && fst (solveCube (makeMoves [R, L, D, F, D, D, U, L', R, R, L, D, D, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', B, B, L, R', D, D, R, L', B, F, F, D', U', L, D, U', B', R, D, U] getSolvedCube)) == getSolvedCube
    && fst (solveCube (makeMoves [F, U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', D', U'] getSolvedCube)) == getSolvedCube
    && fst (solveCube (makeMoves [U', B, B, L, R', D, D, R, L', B, F, F, R, L, F, B', D', U', L, D, U', B', R, D, U] getSolvedCube)) == getSolvedCube