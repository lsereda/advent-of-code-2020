import Data.List.Split (splitOn)
import qualified Data.Map as Map

parseInput :: String -> Map.Map Int Int
parseInput = Map.fromList . flip zip [1..] . map read . splitOn ","

gameIterate :: Int -> Int -> Int -> Map.Map Int Int -> Int
gameIterate lastNum index lastIndex memory
    | index == lastIndex = newNum
    | otherwise          = gameIterate newNum (index + 1) lastIndex $! Map.insert lastNum (index - 1) memory
    where
      newNum = if Map.member lastNum memory then index - 1 - (memory Map.! lastNum) else 0

solveTask :: String -> Int -> Int
solveTask input n = gameIterate (last inputAsList) (length inputAsList + 1) n parsedInput
                    where
                      parsedInput = parseInput input
                      inputAsList = (map read . splitOn ",") input

firstTask :: String -> Int
firstTask input = solveTask input 2020

secondTask :: String -> Int
secondTask input = solveTask input 30000000

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ firstTask input
    print $ secondTask input