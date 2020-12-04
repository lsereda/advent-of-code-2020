parseInput :: String -> [String]
parseInput = lines

route :: [String] -> Int -> Int -> [(Int, Int)]
route input m n = take number $ iterate f (m, n)
                  where
                    number   = div (length input - 1) n
                    width    = (length . head) input
                    f (x, y) = (mod (x + m) width, y + n) 

solveTask :: [String] -> Int -> Int -> Int
solveTask input m n = countTrees $ map getField (route input m n)
                      where
                        getField (x, y) = input !! y !! x
                        countTrees      = length . filter (=='#')

firstTask :: [String] -> Int
firstTask input = solveTask input 3 1

secondTask :: [String] -> Int
secondTask input = product [solveTask input m n | (m, n) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]

main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput