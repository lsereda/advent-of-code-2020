import Control.Arrow
import Data.List
import Data.Set (fromList, member)

parseInput :: String -> [(String, String)]
parseInput = map (splitAt 7) . lines

getIndex :: Char -> Char -> String-> Int
getIndex left right str = fst $ foldl f (0, 2 ^ length str) str
                          where
                            f (d, u) ch
                                | ch == left = (d, (d + u) `div` 2)
                                | otherwise  = ((d + u) `div` 2, u)

rowIndex :: String -> Int
rowIndex = getIndex 'F' 'B'

columnIndex :: String -> Int
columnIndex = getIndex 'L' 'R'

getSeatId :: (String, String) -> Int
getSeatId = uncurry (+) . (multRowIndex *** columnIndex)
            where
              multRowIndex = (*8) . rowIndex

firstTask :: [(String, String)] -> Int
firstTask = maximum . map getSeatId

secondTask :: [(String, String)] -> Int
secondTask input = head [x | x <- [0..1024], isGoodSeat x]
                   where 
                     allIds       = fromList $ map getSeatId input
                     isGoodSeat x = not (member x allIds) && member (x + 1) allIds && member (x - 1) allIds

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput