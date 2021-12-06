import System.Environment
import Data.Strings
import Data.Map (Map, empty, insertWith, toList, elems)

type FishMap = Map Int Int
type FishPair = (Int, Int)

main = do
    args <- getArgs
    let filename = head args
    let days = (read::String->Int) $ args !! 1
    content <- readFile filename
    let cLines = lines content
    let fishes = map read $ strSplitAll "," $ head cLines

    let initMap = fromInit fishes empty
    let fishMap = modelFish days initMap
    let result = foldl (+) 0 $ elems fishMap

    putStrLn $ show $ result


fromInit :: [Int] -> FishMap -> FishMap
fromInit [] fishMap = fishMap
fromInit fishes fishMap = fromInit b $ insertWith (+) a 1 fishMap
    where a = head fishes
          b = tail fishes


fromPairs :: [FishPair] -> FishMap -> FishMap
fromPairs [] fishMap = fishMap
fromPairs (x:xs) fishMap = fromPairs xs $ insertWith (+) (fst x) (snd x) fishMap


nextDay :: FishMap -> FishMap
nextDay fishMap = newMap
    where fishList = concat $ map nextPair $ toList fishMap
          newMap = fromPairs fishList empty


nextPair :: FishPair -> [FishPair]
nextPair (0, count) = [(6, count), (8, count)]
nextPair (day, count) = [(day-1, count)]


modelFish :: Int -> FishMap -> FishMap
modelFish 0 dayZero = dayZero
modelFish day dayZero = nextDay $ modelFish (day -1) dayZero
