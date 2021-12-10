import System.Environment
import Data.Char
import Data.List

type Coord = (Int, Int)
type Heightmap = [[Int]]

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    let heightmap = map (map digitToInt) cLines

    let rows = length heightmap
    let cols = length $ head heightmap

    let lowpoints = findLowPoints (0,0) (rows, cols) heightmap []

    putStrLn $ show $ foldl (+) 0 $ map (\x -> x+1) lowpoints


findLowPoints :: Coord -> Coord -> Heightmap -> [Int] -> [Int]
findLowPoints (i,j) (rows,cols) heightmap lowpoints
  | j < cols = findLowPoints (i,j+1) (rows,cols) heightmap newLowpoints
  | i < rows = findLowPoints (i+1,0) (rows,cols) heightmap lowpoints
  | otherwise = lowpoints
  where newLowpoints = if foundLowpoint then lowpoint:lowpoints
                                        else lowpoints
                                            where foundLowpoint = isLowpoint (i,j) (rows,cols) heightmap
                                                  lowpoint = (heightmap !! i) !! j


isLowpoint :: Coord -> Coord -> Heightmap -> Bool
isLowpoint (i,j) (rows,cols) heightmap
  | i >= rows || i < 0 = False
  | j >= cols || j < 0 = False
  | otherwise = foldl (&&) True $ map (\x -> x > curr) neighbors
  where curr = (heightmap !! i) !! j
        neighbors = findNeighbors (i,j) (rows,cols) heightmap


findNeighbors :: Coord -> Coord -> Heightmap -> [Int]
findNeighbors (i,j) (rows,cols) heightmap = concat [horN, verN]
    where horN = map (row !!) $ filter (\x -> x < cols && x >= 0) [j-1,j+1]
          verN = map (col !!) $ filter (\x -> x < rows && x >= 0) [i-1,i+1]
          row = heightmap !! i
          col = (transpose heightmap) !! j
