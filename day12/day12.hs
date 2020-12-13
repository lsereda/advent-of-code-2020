import Control.Arrow ((***))

type Instruction = (Char, Int)

type Coords = (Int, Int)

parseInput :: String -> [Instruction]
parseInput = map ((head *** read) . splitAt 1) . lines

walkByDegree :: Int -> Int -> Coords -> Coords
walkByDegree deg n (a, b)
    | deg `mod` 360 == 0   = (a, b + n)
    | deg `mod` 360 == 90  = (a - n, b)
    | deg `mod` 360 == 180 = (a, b - n)
    | deg `mod` 360 == 270 = (a + n, b)

firstWalk :: [Instruction] -> Int -> Coords -> Coords
firstWalk [] _ curr          = curr
firstWalk (z:zs) deg (a, b)  = case z of
    ('N', n) -> firstWalk zs deg (a, b + n)
    ('S', n) -> firstWalk zs deg (a, b - n)
    ('E', n) -> firstWalk zs deg (a + n, b)
    ('W', n) -> firstWalk zs deg (a - n, b)
    ('L', n) -> firstWalk zs (deg + n) (a, b)
    ('R', n) -> firstWalk zs (deg - n) (a, b)
    ('F', n) -> firstWalk zs deg (afterWalk n)
    where
      afterWalk n = walkByDegree deg n (a, b)      

manhattanDist :: Coords -> Coords -> Int
manhattanDist (a, b) (x, y) = abs (a - x) + abs (b - y)

firstTask :: [Instruction] -> Int
firstTask input = manhattanDist (0, 0) result
                  where
                    result = firstWalk input 270 (0, 0)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f x = (head . drop n) (iterate f x) 

rotateRight :: Int -> Coords -> Coords
rotateRight deg (x, y) = applyNTimes (deg `div` 90) rotate90 (x, y)
                         where
                           rotate90 (a, b) = (b, -a)

secondWalk :: [Instruction] -> Coords -> Coords -> Coords
secondWalk [] curr _            = curr
secondWalk (z:zs) (a, b) (x, y) = case z of
    ('N', n) -> secondWalk zs (a, b) (x, y + n)
    ('S', n) -> secondWalk zs (a, b) (x, y - n)
    ('E', n) -> secondWalk zs (a, b) (x + n, y)
    ('W', n) -> secondWalk zs (a, b) (x - n, y)
    ('L', n) -> secondWalk zs (a, b) (rotateRight (360 - n) (x, y))
    ('R', n) -> secondWalk zs (a, b) (rotateRight n (x, y))
    ('F', n) -> secondWalk zs (a + n * x, b + n * y) (x, y)

secondTask :: [Instruction] -> Int
secondTask input = manhattanDist (0, 0) result
                   where
                     result = secondWalk input (0, 0) (10, 1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput