import System.Environment
import Data.Strings

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    let horPos = map (read::String->Int) $ strSplitAll "," $ head cLines

    -- putStrLn $ show $ (findTurningPoint 0 0 horPos)
    -- putStrLn $ show $ calcFuel 5 horPos
    putStrLn $ show $ calcFuel (findTurningPoint 0 0 horPos) horPos

-- counter, list, counter
findTurningPoint :: Int -> Int -> [Int] -> Int
findTurningPoint counter minPos horPos
  | counter >= length horPos = minPos
  | minFuel > currFuel = findTurningPoint (counter+1) counter horPos
  | otherwise = findTurningPoint (counter+1) minPos horPos
  where minFuel = calcFuel minPos horPos
        currFuel = calcFuel counter horPos


calcFuel :: Int -> [Int] -> Int
calcFuel index horPos = foldl (+) 0 $ map (calcFuelForIndex index) horPos


calcFuelForIndex :: Int -> Int -> Int
calcFuelForIndex fromIndex index = calcIncreasing $ abs(index - fromIndex)


calcIncreasing :: Int -> Int
calcIncreasing 0 = 0
calcIncreasing a = a + calcIncreasing (a-1)
