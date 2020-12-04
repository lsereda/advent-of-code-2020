import Data.List
import Data.List.Split

type ParsedData = (Int, Int, Char, String)

parseInput :: String -> [ParsedData]
parseInput = map (toCertainForth . splitOneOf "-: ") . lines
              where
                toCertainForth :: [String] -> ParsedData
                toCertainForth [a, b, c, d, e] = (read a, read b, head c, e)

countOccurs :: Char -> String -> Int
countOccurs ch = length . filter (==ch)

isCorrectFirst :: ParsedData -> Bool
isCorrectFirst (lower, upper, ch, str) = countOccurs ch str >= lower && countOccurs ch str <= upper

isCorrectSecond :: ParsedData -> Bool
isCorrectSecond (first, second, ch, str) = (str !! (first - 1) == ch) /= (str !! (second - 1) == ch)
  
solveTask :: (ParsedData -> Bool) -> [ParsedData] -> Int
solveTask f input = length $ filter f input

firstTask :: [ParsedData] -> Int
firstTask = solveTask isCorrectFirst

secondTask :: [ParsedData] -> Int
secondTask = solveTask isCorrectSecond

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ (show . firstTask) parsedInput
    print $ (show . secondTask) parsedInput