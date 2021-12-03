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

    let lineCount = length cLines

    let (gammaArr, epsilonArr) = buildEpGamma lineCount starter intLists

    let oxygen = filterWith 0 gammaArr intLists
    let co2 = binToInt $ reverse $ filterWith 0 epsilonArr intLists

    -- TODO: gammat ja epsilonit tarvii laskea aina uudestaan kun listasta
    -- poistuu rivejä
    putStrLn $ show $ oxygen


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
toEpsilon countInt numInt = if (num / count) > 0.5 then 0
                                                   else 1
                                                       where
                                                           num = fromIntegral numInt
                                                           count = fromIntegral countInt


-- copied from StackOverflow because I am lazy
binToInt :: [Int] -> Int -- the problem is here
binToInt [] = 0
binToInt (x : xs) = x + 2 * binToInt xs
