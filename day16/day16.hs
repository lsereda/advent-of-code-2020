import Control.Arrow (second)
import Data.List.Split (splitOn)
import Data.List (sortOn, isSubsequenceOf)
import qualified Data.Map as Map

type Range = (Int, Int)

type Rules = Map.Map String [Range]

type Ticket = [Int]

type Input = (Rules, Ticket, [Ticket])

parseRanges :: String -> [Range]
parseRanges input = map toRange ranges
                    where
                      ranges        = splitOn " or " input
                      toRange       = toPair . map read . splitOn "-"
                      toPair [x, y] = (x, y)

parseRule :: String -> (String, [Range])
parseRule input = (name, ranges)
                  where
                    parts  = splitOn ":" input
                    name   = head parts
                    ranges = parseRanges $ parts !! 1

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseInput :: String -> (Rules, Ticket, [Ticket])
parseInput input = (rules parts, myTicket parts, nearbyTickets parts)
                   where
                     parts                   = (map lines . splitOn "\n\n") input
                     rules         [x, y, z] = Map.fromList $ map parseRule x
                     myTicket      [x, y, z] = (parseTicket . last) y
                     nearbyTickets [x, y, z] = (map parseTicket . tail) z

isValid :: Int -> Range -> Bool
isValid num (x, y) = num >= x && num <= y

isTicketValid :: Ticket -> [Range] -> Bool
isTicketValid ticket rules = all (\x -> any (isValid x) rules) ticket

firstTask :: Input -> Int
firstTask (rawRules, _, nearbyTickets) = sum [x | t <- nearbyTickets, x <- t, not $ any (isValid x) rules]
                                         where
                                           rules = (concat . Map.elems) rawRules 

validTickets :: Input -> [Ticket]
validTickets (rawRules, _, nearbyTickets) = [t | t <- nearbyTickets, isTicketValid t rules]
                                            where
                                              rules = (concat . Map.elems) rawRules

toIndexedTickets :: [Ticket] -> [Ticket]
toIndexedTickets tickets = map (\x -> [a | (a, b) <- enumerated, b == x]) [0..len - 1]
                           where
                             enumerated = tickets >>= flip zip [0..]
                             len        = length $ head tickets

fittedRows :: Input -> Map.Map String [Int]
fittedRows input@(rules, _, _) = Map.map f rules
                                 where
                                   indexed  = zip [0..] $ (toIndexedTickets . validTickets) input
                                   f [a, b] = [i | (i, x) <- indexed, all (\y -> isValid y a || isValid y b) x]
                                       
createAssociations :: [(String, [Int])] -> [(String, Int)] -> [(String, Int)]
createAssociations [] acc = acc
createAssociations ((str, [num]):xs) acc = createAssociations withoutNum $ (str, num) : acc
                                           where
                                             removeNum  = filter (/= num)
                                             withoutNum = map (second removeNum) xs

secondTask :: Input -> Int
secondTask input@(rules, myTicket, nearbyTickets) = product $ map ((myTicket!!) . snd) departureAssocs
                                                     where
                                                       posAssocs       = Map.toList $ fittedRows input
                                                       sortedAssocs    = sortOn (length . snd) posAssocs
                                                       finalAssocs     = createAssociations sortedAssocs []
                                                       departureAssocs = filter (isSubsequenceOf "departure" . fst) finalAssocs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput