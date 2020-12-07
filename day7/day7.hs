import Data.Map (Map, fromList, (!), foldWithKey)

type RightSide = (Int, String)

type Rule = (String, [RightSide])

type RulesMap = Map String [RightSide]

parseRightSide :: [String] -> [RightSide]
parseRightSide []                        = []
parseRightSide ["no", "other", "bags."]  = []
parseRightSide (num:x:y:_:rest)          = (read num, x ++ y) : parseRightSide rest

parseSingleLine :: [String] -> Rule
parseSingleLine (x:y:_:"contain":rest) = (x ++ y, parseRightSide rest)

parseInput :: String -> RulesMap
parseInput = fromList . map (parseSingleLine . words) . lines

contain :: RulesMap -> String -> Rule -> Bool
contain rulesMap str (left, right) = str == left || any (contain rulesMap str) insideRules
                                     where
                                       insideRules = map (\(_, y) -> (y, rulesMap ! y)) right

firstTask :: RulesMap -> String -> Int
firstTask rulesMap x = foldWithKey f 0 rulesMap - 1
                       where
                         f k v acc = if contain rulesMap x (k, v) then acc + 1 else acc

secondTask :: RulesMap -> String -> Int
secondTask rulesMap x = sum $ map calculate right
                        where
                          right = rulesMap ! x
                          calculate (x, y) = x * (1 + secondTask rulesMap y)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput "shinygold"
    print $ secondTask parsedInput "shinygold"