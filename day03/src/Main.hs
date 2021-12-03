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

    let sumBitArr::[Int] = foldl (zipWith (+)) starter intLists
    let lineCount = length cLines

    let gammaArr = map (toGamma lineCount) sumBitArr
    let epsilonArr = map (toEpsilon lineCount) sumBitArr

    let gamma = binToInt $ reverse gammaArr
    let epsilon = binToInt $ reverse epsilonArr

    putStrLn $ show $ gamma * epsilon


-- Most common
toGamma :: Int -> Int -> Int
toGamma countInt numInt = if (num / count) > 0.5 then 1
                                                 else 0
                                                     where
                                                         num = fromIntegral numInt
                                                         count = fromIntegral countInt

-- Least common
toEpsilon :: Int -> Int -> Int
toEpsilon countInt numInt = if (num / count) > 0.5 then 0
                                                   else 1
                                                       where
                                                           num = fromIntegral numInt
                                                           count = fromIntegral countInt


-- copied from StackOverflow because I am lazy
binToInt :: [Int] -> Int -- the problem is here
binToInt [] = 0
binToInt (x : xs) = x + 2 * binToInt xs
