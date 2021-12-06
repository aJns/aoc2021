import System.Environment
import Data.Strings

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let fishes = map read $ strSplitAll "," $ head cLines

    putStrLn $ show $ length $ modelFish 80 fishes



nextDay :: Int -> [Int]
nextDay 0 = [6, 8]
nextDay a = [a-1]


modelFish :: Int -> [Int] -> [Int]
modelFish 0 dayZero = dayZero
modelFish day dayZero = concat $ map nextDay $ modelFish (day -1) 
