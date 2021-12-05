import System.Environment
import Data.Strings

type Coord = (Int, Int)
type CoordPair = (Coord, Coord)


main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    -- let startEndPairs = map parsePair cLines

    putStrLn $ show $ map (parsePair . splitLine) cLines


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
