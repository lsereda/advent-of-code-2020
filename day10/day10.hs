import Data.List (sort)
import qualified Data.Map as Map

parseInput :: String -> [Int]
parseInput = map read . lines

countOccurs :: Eq a => a -> [a] -> Int
countOccurs el = length . filter(==el)

firstTask :: [Int] -> Int
firstTask input = countOccurs 1 differences * (countOccurs 3 differences + 1)
                  where
                    sorted      = sort (0:input)
                    differences = zipWith (-) (tail sorted) sorted

getOrElse :: Ord a => Map.Map a b -> b -> a -> b
getOrElse map def key
    | Map.member key map = map Map.! key
    | otherwise          = def

secondTask :: [Int] -> Int
secondTask input = connectionsMap Map.! maximum input
                   where
                     sorted         = sort (0:input)
                     value x acc    = sum $ map (getOrElse acc 0) [x - 1, x - 2, x - 3]
                     f acc 0        = Map.insert 0 1 acc
                     f acc x        = Map.insert x (value x acc) acc
                     connectionsMap = foldl f Map.empty sorted

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput