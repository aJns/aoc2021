import System.Environment
import Data.Strings

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    let horPos = map read $ strSplitAll "," $ head cLines

    putStrLn $ show $ calcFuel (findTurningPoint 0 horPos) horPos

-- counter, list, counter
findTurningPoint :: Int -> [Int] -> Int
findTurningPoint a [] = a 
findTurningPoint counter horPos
  | prevFuel < currFuel = counter
  | otherwise = findTurningPoint (counter+1) horPos
  where prevFuel = calcFuel prev horPos
        currFuel = calcFuel curr horPos
        prev = horPos !! (counter)
        curr = horPos !! (counter + 1)


calcFuel :: Int -> [Int] -> Int
calcFuel index horPos = foldl (+) 0 $ map (\x -> abs(x - index)) horPos
