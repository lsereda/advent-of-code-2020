import Data.List
import Data.List.Split
import Data.Map (Map, fromList, member, (!), toList)
import Data.Maybe
import Text.Regex (matchRegex, mkRegexWithOpts)

type ParsedData = (String, String)

parseInput :: String -> [[ParsedData]] 
parseInput = map (extractPairs . words . mergeLines) . groupByDocs . lines
             where
               groupByDocs      = groupBy (\x y -> y /= "")
               mergeLines       = (>>= (' ':))
               list2Pair [x, y] = (x, y)
               extractPairs     = map (list2Pair . splitOn ":")

allNecessaryFields :: [String]
allNecessaryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "hgt"]

necessaryFieldPassports :: [[ParsedData]] -> [Map String String]
necessaryFieldPassports = filter isValid . map fromList
                       where
                         isValid fieldsMap = all (`member` fieldsMap) allNecessaryFields

firstTask :: [[ParsedData]] -> Int
firstTask = length . necessaryFieldPassports

matches :: String -> String -> Bool
matches pattern elem = isJust $ matchRegex (mkRegexWithOpts pattern True False) elem

isGoodNumber :: String -> Int -> (Int, Int) -> Bool
isGoodNumber str n (low, up) = length str == n && number >= low && number < up
                               where
                                 number = read str 

byr :: String -> Bool
byr str = isGoodNumber str 4 (1920, 2003)

iyr :: String -> Bool
iyr str = isGoodNumber str 4 (2010, 2021)

eyr :: String -> Bool
eyr str = isGoodNumber str 4 (2020, 2031)

hcl :: String -> Bool
hcl str = matches "^#[0-9a-f]{6}$" str

hgt :: String -> Bool
hgt str
    | matches "^[0-9]{3}cm$" str = isGoodNumber (take 3 str) 3 (150, 194)
    | matches "^[0-9]{2}in$" str = isGoodNumber (take 2 str) 2 (59, 77)
    | otherwise                  = False

ecl :: String -> Bool
ecl str = str `elem` ["amb", "blu", "brn" ,"gry", "grn", "hzl", "oth"]

pid :: String -> Bool
pid str = matches "^[0-9]{9}$" str

cid :: String -> Bool
cid _ = True

funcMap :: Map String (String -> Bool)
funcMap = fromList [("byr", byr), ("iyr", iyr), ("eyr", eyr), ("hcl", hcl), ("hgt", hgt), ("ecl", ecl), ("pid", pid), ("cid", cid)]

secondTask :: [[ParsedData]] -> Int
secondTask = length . filter isValid . necessaryFieldPassports
             where
                applyFunc (x, y)  = (funcMap ! x) y
                isValid fieldsMap = all (==True) $ (map (applyFunc) . toList) fieldsMap

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput