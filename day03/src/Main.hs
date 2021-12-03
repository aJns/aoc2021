{-# LANGUAGE ScopedTypeVariables #-}
import System.Environment
import Data.Char

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let intLists = map (map digitToInt) cLines

    let bitCount = length $ head intLists
    let starter = replicate bitCount 0

    let lineCount = length intLists

    let oxygenArr = buildOxygen 0 starter intLists
    let co2Arr = buildCo2 0 starter intLists

    let oxygen = binToInt $ reverse $ oxygenArr
    let co2 = binToInt $ reverse $ co2Arr

    putStrLn $ show $ oxygen * co2


buildOxygen :: Int -> [Int] -> [[Int]] -> [Int]
buildOxygen _ _ (a:[]) = a
buildOxygen index starter numbers = buildOxygen (index + 1) starter newNumbers
    where newNumbers = filter (\x -> (x !! index) == (filterArr !! index)) numbers
          (filterArr, _) = buildEpGamma (length numbers) starter numbers


buildCo2 :: Int -> [Int] -> [[Int]] -> [Int]
buildCo2 _ _ (a:[]) = a
buildCo2 index starter numbers = buildCo2 (index + 1) starter newNumbers
    where newNumbers = filter (\x -> (x !! index) == (filterArr !! index)) numbers
          (_, filterArr) = buildEpGamma (length numbers) starter numbers


buildEpGamma :: Int -> [Int] -> [[Int]] -> ([Int], [Int])
buildEpGamma lineCount starter intLists = (gammaArr, epsilonArr)
    where 
        gammaArr = map (toGamma lineCount) sumBitArr
        epsilonArr = map (toEpsilon lineCount) sumBitArr
        sumBitArr::[Int] = foldl (zipWith (+)) starter intLists


filterWith :: Int -> [Int] -> [[Int]] -> [Int]
filterWith _ _ (a:[]) = a
filterWith index filterArr numbers = filterWith (index + 1) (tail filterArr) newNumbers
    where newNumbers = filter (\x -> (x !! index) == (head filterArr)) numbers

-- Most common
toGamma :: Int -> Int -> Int
toGamma countInt numInt = if (num / count) >= 0.5 then 1
                                                  else 0
                                                      where
                                                          num = fromIntegral numInt
                                                          count = fromIntegral countInt

-- Least common
toEpsilon :: Int -> Int -> Int
toEpsilon countInt numInt = if (num / count) >= 0.5 then 0
                                                    else 1
                                                        where
                                                            num = fromIntegral numInt
                                                            count = fromIntegral countInt


-- copied from StackOverflow because I am lazy
binToInt :: [Int] -> Int -- the problem is here
binToInt [] = 0
binToInt (x : xs) = x + 2 * binToInt xs
