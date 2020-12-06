import Data.List.Split
import Data.Set (fromList, toList)

parseInput :: String -> [String]
parseInput = endBy "\n\n"

firstTask :: [String] -> Int
firstTask input = sum $ map (length . fromList . removeNewLines) input
                  where
                    removeNewLines = Prelude.filter (/='\n')

secondTask :: [String] -> Int
secondTask input = sum $ map (length . removeDuplicates . removeBadChars) input
                   where
                     countOccurs ch      = length . filter (==ch)
                     countLines list     = countOccurs '\n' list + 1
                     removeBadChars list = [ch | ch <- list, countOccurs ch list == countLines list, ch /= '\n']
                     removeDuplicates    = toList . fromList

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ (show . secondTask) parsedInput