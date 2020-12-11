import qualified Data.Map as Map

type SeatIndex = (Int, Int)

type SeatMap = Map.Map SeatIndex Char

type AdjacentSeatMap = Map.Map SeatIndex [SeatIndex]

type FieldTransformer = SeatIndex -> Char -> SeatMap -> AdjacentSeatMap -> Char

addLineToMap :: SeatMap -> Int -> String -> SeatMap
addLineToMap map index line = foldl f map (zip line [0..])
                              where
                                f acc (x, y) = Map.insert (index, y) x acc

parseInput :: String -> SeatMap
parseInput input = foldl f Map.empty (zip (lines input) [0..])
                   where
                     f acc (x, y) = addLineToMap acc y x 

getOrElse :: Ord a => Map.Map a b -> b -> a -> b
getOrElse map def key
    | Map.member key map = map Map.! key
    | otherwise          = def

countOccurs :: Eq a => a -> [a] -> Int
countOccurs el = length . filter (==el)

transformMap :: AdjacentSeatMap -> FieldTransformer -> SeatMap -> SeatMap
transformMap adjacentMap transformer seatMap = Map.mapWithKey f seatMap
                                               where
                                                 f k v = transformer k v seatMap adjacentMap

runUntilStabilize :: SeatMap -> (SeatMap -> SeatMap) -> SeatMap
runUntilStabilize input f = (fst . head) $ dropWhile (uncurry (/=)) iterations
                            where
                              maps       = iterate f input
                              iterations = zip maps (tail maps)  

transformSeat :: Int -> FieldTransformer
transformSeat n (x, y) ch seatMap adjacentMap
    | ch == 'L' && countOccupied == 0 = '#'
    | ch == '#' && countOccupied >= n = 'L'
    | otherwise                       = ch
    where
      adjacentTypes = map (getOrElse seatMap '.') (adjacentMap Map.! (x, y))
      countOccupied = countOccurs '#' adjacentTypes

firstTaskAdjacentSeats :: SeatMap -> AdjacentSeatMap
firstTaskAdjacentSeats = Map.mapWithKey f
                         where
                           f k '.'    = []
                           f (x, y) _ = [(a, b) | a <- [x - 1, x, x + 1], b <- [y - 1, y, y + 1], (a, b) /= (x, y)] 

firstTask :: SeatMap -> Int
firstTask input = countOccurs '#' (Map.elems firstIndentical)
                  where
                    adjacentMap     = firstTaskAdjacentSeats input
                    firstIndentical = runUntilStabilize input (transformMap adjacentMap (transformSeat 4)) 

addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair (x, y) (a, b) = (x + a, y + b)

findSeat :: SeatIndex -> SeatMap -> (Int, Int) -> SeatIndex
findSeat x seatMap y
    | not $ Map.member added seatMap = added
    | seatMap Map.! added == '.'     = findSeat added seatMap y
    | otherwise                      = added
    where
      added = addPair x y

secondTaskAdjacentSeats :: SeatMap -> AdjacentSeatMap
secondTaskAdjacentSeats seatMap = Map.mapWithKey f seatMap
                                  where
                                    adjacentHelper = [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], (a, b) /= (0, 0)] 
                                    f k '.'        = []
                                    f (x, y) _     = map (findSeat (x, y) seatMap) adjacentHelper

secondTask :: SeatMap -> Int
secondTask input = countOccurs '#' (Map.elems secondIdentical)
                  where
                    adjacentMap     = secondTaskAdjacentSeats input
                    secondIdentical = runUntilStabilize input (transformMap adjacentMap (transformSeat 5)) 

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput