import System.Environment
import Data.Map (Map, insert, insertWith)
import qualified Data.Map as Map
import Data.Strings


type PairMap = Map (Char,Char) Char

main = do
    args <- getArgs
    let filename = head args
    let steps = (read::String->Int) $ args !! 1
    content <- readFile filename
    let cLines = lines content

    let template = head cLines
    let pairLines = drop 2 cLines
    let pairMap = parseToMap Map.empty pairLines
    let inserted = insertForSteps pairMap steps template
    let hist = buildHistogram Map.empty inserted
    let histStart = head $ Map.elems hist
    let (min,max) = findMinMax (histStart, histStart) $ Map.elems hist


    -- TODO: Possible solutions to part 2
    -- * MergeSort, divide into chunks until we have two chars, then replace
    -- * Cache solutions, ie if step 1->2 gives us asd=>asdasd, then add
    --   asd,asdasd to the map

    putStrLn $ show $ max-min


findMinMax :: (Int,Int) -> [Int] -> (Int,Int)
findMinMax (min,max) [] = (min,max)
findMinMax (min,max) (val:vals) = findMinMax (newMin,newMax) vals
    where newMin = if val < min then val else min
          newMax = if val > max then val else max


buildHistogram :: Map Char Int -> String -> Map Char Int
buildHistogram hist [] = hist
buildHistogram hist (x:xs) = buildHistogram newHist xs
    where newHist = insertWith (+) x 1 hist


insertForSteps :: PairMap -> Int -> String -> String
insertForSteps pairMap 0 polymer = polymer
insertForSteps pairMap steps polymer = insertForSteps pairMap (steps-1) newPolymer
    where newPolymer = insertPairs "" pairMap polymer


insertPairs :: String -> PairMap -> String -> String
insertPairs outStr pairMap (a:[]) = reverse $ filter (\x -> not (x == ' ')) $ a:outStr
insertPairs outStr pairMap (a:b:rest) = insertPairs newStr pairMap (b:rest)
    where newStr = newMiddle:(a:outStr)
          newMiddle = Map.findWithDefault ' ' (a,b) pairMap



parseToMap :: PairMap -> [String] -> PairMap
parseToMap pairMap [] = pairMap
parseToMap pairMap (x:xs) = parseToMap newMap xs
    where newMap = insert key value pairMap
          splitted = strSplit " -> " x
          key = stringToPair $ fst splitted
          value = head $ snd splitted


stringToPair :: String -> (Char, Char)
stringToPair (a:b:_) = (a,b)
