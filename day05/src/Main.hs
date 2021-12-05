import System.Environment
import Data.Strings
import Data.Map (Map, empty, insertWith, (!), elems)

type Coord = (Int, Int)
type CoordPair = (Coord, Coord)


main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    let startEndPairs = map (parsePair . splitLine) cLines
    -- let filtered = filter (\((x1,y1),(x2,y2)) -> (x1 == x2) || (y1 == y2)) startEndPairs

    let coordLines = concat $ map fillInLine startEndPairs
    let coordMap = constructMap coordLines empty

    let values = elems coordMap
    let atLeast2 = length $ filter (\x -> x >= 2) $ values

    putStrLn $ show $ atLeast2


constructMap :: [Coord] -> Map Coord Int -> Map Coord Int
constructMap [] coordMap = coordMap
constructMap coords coordMap = constructMap (tail coords) newMap
    where newMap = insertWith (+) (head coords) 1 coordMap


splitLine :: String -> [[String]] 
splitLine lineStr = coordPair
    where coordPair = map (strSplitAll ",") splitted
          splitted = strSplitAll " -> " lineStr


parsePair :: [[String]] -> CoordPair
parsePair [[sx1,sy1],[sx2,sy2]] = ((x1,y1),(x2,y2))
    where x1 = read sx1
          y1 = read sy1
          x2 = read sx2
          y2 = read sy2


fillInLine :: CoordPair -> [Coord]
fillInLine ((x1,y1),(x2,y2))
  | y1 == y2 = row
  | x1 == x2 = col
  | otherwise = diag
  where row 
          | x2 >= x1 = map (\x -> (x, y1)) [x1..x2]
          | x2 < x1 = map (\x -> (x, y1)) [x2..x1]
        col 
          | y2 >= y1 = map (\y -> (x1, y)) [y1..y2]
          | y2 < y1 = map (\y -> (x1, y)) [y2..y1]
        diag
          | y2 >= y1 && x2 >= x1 = zip [x1..x2]           [y1..y2]
          | y2 >= y1 && x2 < x1  = zip (reverse [x2..x1]) [y1..y2]
          | y2 < y1 && x2 >= x1  = zip [x1..x2]           (reverse [y2..y1])
          | y2 < y1 && x2 < x1   = zip (reverse [x2..x1]) (reverse [y2..y1])














