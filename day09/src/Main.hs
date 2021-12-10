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

    let basins = map (\x -> findBasin ([x], []) heightmap) lowpoints
    let lengths = map (length . nub) basins
    let big3 = take 3 $ reverse $ sort lengths

    putStrLn $ show $ foldl (*) 1 big3


findBasin :: ([Coord], [Coord]) -> Heightmap -> [Coord]
findBasin ([], basin) _ = basin
findBasin (toCheck, basin) heightmap = findBasin (newCheck, newBasin) heightmap
  where newCheck = concat [(tail toCheck), filteredNeighborsToCheck]
        newBasin = newPart:basin
        newPart = head toCheck
        filteredNeighborsToCheck = filter (\x -> notElem x basin) neighborsToCheck
        neighborsToCheck = filter (isPartOfBasin newPartVal heightmap) neighbors
        neighbors = findNeighborCoord newPart (rows, cols)
        rows = length heightmap
        cols = length $ head heightmap
        newPartVal = (heightmap !! i) !! j
        i = fst newPart
        j = snd newPart



isPartOfBasin :: Int -> Heightmap -> Coord -> Bool
isPartOfBasin val heightmap neighbor = isPart
  where isPart = neighVal > val && neighVal < 9 
        neighVal = (heightmap !! i) !! j
        i = fst neighbor
        j = snd neighbor


findNeighborCoord :: Coord -> Coord -> [Coord]
findNeighborCoord (i,j) (rows,cols) = concat [hor,ver]
    where horN = filter (\x -> x < cols && x >= 0) [j-1,j+1]
          verN = filter (\x -> x < rows && x >= 0) [i-1,i+1]
          hor = [(i,y) | y <- horN]
          ver = [(x,j) | x <- verN]


findLowPoints :: Coord -> Coord -> Heightmap -> [Coord] -> [Coord]
findLowPoints (i,j) (rows,cols) heightmap lowpoints
  | j < cols = findLowPoints (i,j+1) (rows,cols) heightmap newLowpoints
  | i < rows = findLowPoints (i+1,0) (rows,cols) heightmap lowpoints
  | otherwise = lowpoints
  where newLowpoints = if foundLowpoint then lowpoint:lowpoints
                                        else lowpoints
                                            where foundLowpoint = isLowpoint (i,j) (rows,cols) heightmap
                                                  lowpoint = (i,j)


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
