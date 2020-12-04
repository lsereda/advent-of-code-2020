parseInput :: String -> [Int]
parseInput = map read . lines

firstPairThatSums :: [Int] -> (Int, Int)
firstPairThatSums xxs = head [(x, y) | x <- xxs, y <- xxs, x + y == 2020]

firstTask :: [Int] -> Int
firstTask xxs = uncurry (*) pair
                where
                  pair = firstPairThatSums xxs

firstTripletThatSums :: [Int] -> (Int, Int, Int)
firstTripletThatSums xxs = head [(x, y, z) | x <- xxs, y <- xxs, z <- xxs, x + y + z == 2020]

secondTask :: [Int] -> Int
secondTask xxs = fst3 triplet * snd3 triplet * trd3 triplet
                 where
                   triplet = firstTripletThatSums xxs
                          
fst3 :: (a, a, a) -> a
fst3 (x, _, _) = x

snd3 :: (a, a, a) -> a
snd3 (_, y, _) = y

trd3 :: (a, a, a) -> a
trd3 (_, _, z) = z 

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input 
    print $ firstTask parsedInput
    print $ secondTask parsedInput