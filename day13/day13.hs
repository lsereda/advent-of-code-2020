import Data.List.Split (splitOn)

parseInput :: String -> (Integer, [String])
parseInput input = (read $ head line, splitOn "," $ last line)
                   where
                     line = lines input

firstTask :: (Integer, [String]) -> Int
firstTask (n, list) = fromInteger $ (fst result - n) * snd result
                      where
                        numbers = [read x | x <- list, x /= "x"]
                        result  = head [(x, y) | x <- [n..], y <- numbers, x `mod` y == 0]

parseSecondTaskInput :: (Integer, [String]) -> [(Integer, Integer)]
parseSecondTaskInput (_, xs) = map (filterZeros . prepare) withoutX
                               where
                                 withTimeStamps     = zip xs [0..]
                                 withoutX           = filter (\(x, y) -> x /= "x") withTimeStamps
                                 filterZeros (x, y) = (if x == y then 0 else x, y)
                                 prepare (x, y)     = (a - y `mod` a, a)
                                                      where
                                                        a = read x

euclidean :: Integer -> Integer -> (Integer, Integer, Integer)
euclidean a b = helper a b 1 0 0 1
                where
                  helper a 0 x y _ _ = (a, x, y)
                  helper a b x y r s = helper b c r s (x - q * r) (y - q * s)
                                       where
                                         c = a `mod` b
                                         q = a `div` b

snd3 :: (a, a, a) -> a
snd3 (_, x, _) = x

solveChineseRestTheorem :: [(Integer, Integer)] -> Integer
solveChineseRestTheorem list = sum (map factor list) `mod` numbersProduct
                               where
                                 numbers        = map snd list
                                 numbersProduct = product numbers
                                 factor (x, y)  = x * coeff y * snd3 (euclidean (coeff y) y)
                                 coeff x        = numbersProduct `div` x

secondTask :: (Integer, [String]) -> Integer
secondTask input = solveChineseRestTheorem coeffs
                   where
                     coeffs = parseSecondTaskInput input

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput