import qualified Data.Map as M
import qualified Data.Set as S

data Op = Acc Int
        | Jmp Int
        | Nop Int

data Status = Looped Int
            | Terminated Int

isTerminated :: Status -> Bool
isTerminated (Terminated _) = True
isTerminated _              = False

statusValue :: Status -> Int
statusValue (Looped x)     = x
statusValue (Terminated x) = x

makeOp :: (String, Int) -> Op
makeOp ("acc", x) = Acc x
makeOp ("jmp", x) = Jmp x
makeOp ("nop", x) = Nop x

parseNumber :: String -> Int
parseNumber ('+':x) = read x
parseNumber x       = read x

parseInput :: String -> M.Map Int Op
parseInput = M.fromList . zip [0..] . map (makeOp . parsePair . words) . lines
             where
               parsePair [x, y] = (x, parseNumber y)

getEntry :: Ord a => a -> M.Map a b -> (a, b)
getEntry x entryMap = (x, entryMap M.! x)

execute :: M.Map Int Op -> Int -> Int -> S.Set Int -> Status
execute opMap acc index set
    | S.member index set        = Looped acc
    | index > M.size opMap - 1  = Terminated acc
    | otherwise = case getEntry index opMap of
                    (i, Acc x)  -> execute opMap (acc + x) (i + 1) (S.insert i set)
                    (i, Jmp x)  -> execute opMap acc (i + x) (S.insert i set)
                    (i, Nop _)  -> execute opMap acc (i + 1) (S.insert i set)

startTaskHelper :: M.Map Int Op -> Status
startTaskHelper opMap = execute opMap 0 0 S.empty

firstTask :: M.Map Int Op -> Int
firstTask opMap = statusValue $ startTaskHelper opMap

secondTask :: M.Map Int Op -> Int
secondTask opMap = (statusValue . head) $ filter isTerminated $ map startTaskHelper changedMaps
                   where
                     changedMaps  = map (\x -> f (getEntry x opMap)) [0..M.size opMap - 1]
                     f (n, Acc _) = opMap
                     f (n, Jmp x) = M.insert n (Nop x) opMap
                     f (n, Nop x) = M.insert n (Jmp x) opMap

main :: IO ()
main = do
    input <- readFile   "input.txt"
    let parsedInput = parseInput input
    print $ firstTask parsedInput
    print $ secondTask parsedInput