import qualified Data.Bits as Bits
import Data.List.Split (splitOn)
import qualified Data.Map as Map

data Instruction = Mask String 
                 | Assignment Int Int

parseAddress :: String -> Int
parseAddress ('m':'e':'m':'[':rest) = read $ init rest

toInstruction :: (String, String) -> Instruction
toInstruction ("mask", n) = Mask n
toInstruction (str, n)    = Assignment (parseAddress str) (read n)

parseInput :: String -> [Instruction]
parseInput = map (toInstruction . toPair . splitOn " = ") . lines
             where
               toPair [x, y] = (x, y)

applyFirstMask :: [(Char, Int)] -> Int -> Int
applyFirstMask [] value            = value
applyFirstMask (('0', n):xs) value = applyFirstMask xs (Bits.clearBit value n)
applyFirstMask (('1', n):xs) value = applyFirstMask xs (Bits.setBit value n)
applyFirstMask (('X', _):xs) value = applyFirstMask xs value

executeFirst :: [Instruction] -> String -> Map.Map Int Int -> Map.Map Int Int
executeFirst [] _ memory        = memory
executeFirst (x:xs) mask memory = case x of
    Mask str             -> executeFirst xs str memory
    Assignment adr value -> executeFirst xs mask (Map.insert adr maskedValue memory)
                            where
                              indexedMask = zip (reverse mask) [0..]
                              maskedValue = applyFirstMask indexedMask value

firstTask :: [Instruction] -> Int
firstTask input = Map.fold (+) 0 memory
                  where
                    memory = executeFirst input "" Map.empty

applySecondMask :: [(Char, Int)] -> Int -> Int
applySecondMask [] value            = value
applySecondMask (('1', n):xs) value = applySecondMask xs (Bits.setBit value n)
applySecondMask ((_, _):xs) value   = applySecondMask xs value

generateAddresses :: Int -> [Int] -> [Int]
generateAddresses x []         = [x]
generateAddresses value (x:xs) = generateAddresses (Bits.setBit value x) xs ++ generateAddresses (Bits.clearBit value x) xs

insertValues :: Map.Map Int Int -> Int -> Int -> String -> Map.Map Int Int
insertValues memory adr value mask = foldl f memory allAddresses
                                     where
                                       f acc x           = Map.insert x value acc
                                       indexedMask       = zip (reverse mask) [0..]
                                       secondMaskApplied = applySecondMask indexedMask adr
                                       xPositions        = map snd $ filter (\(x, y) -> x == 'X') indexedMask
                                       allAddresses      = generateAddresses secondMaskApplied xPositions

executeSecond :: [Instruction] -> String -> Map.Map Int Int -> Map.Map Int Int
executeSecond [] _ memory = memory
executeSecond (x:xs) mask memory = case x of
    Mask str             -> executeSecond xs str memory
    Assignment adr value -> executeSecond xs mask withNewValues
                            where
                              withNewValues = insertValues memory adr value mask

secondTask :: [Instruction] -> Int
secondTask input = Map.fold (+) 0 memory
                  where
                    memory = executeSecond input "" Map.empty

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput