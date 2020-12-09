import Queue

parseInput :: String -> ([Int], [Int])
parseInput = splitAt 25 . map read . lines

firstTask :: ([Int], [Int]) -> Int
firstTask (xs, y:ys)
    | not $ isSum xs y  = y
    | otherwise         = firstTask (tail xs ++ [y], ys)
    where
      isSum zs z = z `elem` [x + y | x <- zs, y <- zs, x /= y]

secondTask :: ([Int], [Int]) -> Int
secondTask (x:xs, ys) = minimum subset + maximum subset
                          where
                            firstTaskResult = firstTask (x:xs, ys)
                            subset          = caterpillar (push x empty) (xs ++ ys) x firstTaskResult

caterpillar :: Queue Int -> [Int] -> Int -> Int -> [Int]
caterpillar queue (x:xs) currSum target
    | currSum == target = toList queue
    | currSum < target  = caterpillar (push x queue) xs (currSum + x) target
    | currSum > target  = caterpillar queueAfterPop (x:xs) (currSum - elem) target
    where
      (elem, queueAfterPop) = pop queue

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput