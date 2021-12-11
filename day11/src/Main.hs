import System.Environment
import Data.Char
import Data.Map (Map, empty, insert, (!), keys, insert, insertWith, member, elems)
import qualified Data.Map as Map

type Coord = (Int, Int)
type OctoMap = Map Coord Int


main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    let octopiStart = map (map digitToInt) cLines
    let octoMap = buildMap empty (0,0) octopiStart

    let rows = length octopiStart
    let cols = length $ head octopiStart
    let octoKeys = map (\i -> [(i,j) | j <- [0..cols-1]]) [0..rows-1]

    putStrLn $ show $ runSteps 0 (octoMap, 0)


mapToList :: [[Coord]] -> OctoMap -> [[Int]]
mapToList keys octomap = map (map (octomap !)) keys


buildMap :: OctoMap -> Coord -> [[Int]] -> OctoMap
buildMap octoMap (i,j) octoList
  | j < cols = buildMap (insert (i,j) octoVal octoMap) (i,j+1) octoList
  | i < rows-1 = buildMap octoMap (i+1, 0) octoList
  | otherwise = octoMap
  where rows = length octoList
        cols = length $ head octoList
        octoVal = (octoList !! i) !! j


runSteps :: Int -> (OctoMap, Int) -> Int
runSteps step (_, 100) = step
runSteps step (octoMap, flashCounter) = runSteps (step+1) $ runStep octoMap


runStep :: OctoMap -> (OctoMap, Int)
runStep octoMap = (nextMap, flashCount)
    where incrMap = Map.map (+ 1) octoMap
          flashedMap = flashAll (keys incrMap) incrMap
          nextMap = Map.map (max 0) flashedMap
          flashes = filter (\x -> x < 0) $ elems flashedMap
          flashCount = length flashes


flashAll :: [Coord] -> OctoMap -> OctoMap
flashAll [] octoMap = octoMap
flashAll (x:xs) octoMap = flashAll toCheck nextMap
    where (nbors, nextMap) = flash octoMap x
          toCheck = nbors ++ xs


flash :: OctoMap -> Coord -> ([Coord], OctoMap)
flash octoMap coord
  | octoMap ! coord > 9 = (neighbors, flashedMap)
  | otherwise = ([], octoMap)
  where flashedMap = incrementNeighbors neighbors negMap
        negMap = insert coord (-9999) octoMap -- asetetaan arvo negatiiviseksi = on flashatty
        neighbors = findNeighbors octoMap coord


findNeighbors :: OctoMap -> Coord -> [Coord]
findNeighbors octoMap (i,j) = onlyN
  where neighbors = [(x,y) | x <- [i-1..i+1], y <- [j-1..j+1]]
        filtN = filter (\x -> member x octoMap) neighbors
        onlyN = filter (\x -> not ((i,j) == x)) filtN


incrementNeighbors :: [Coord] -> OctoMap -> OctoMap
incrementNeighbors [] octoMap = octoMap
incrementNeighbors (x:xs) octoMap = incrementNeighbors xs newMap
    where newMap = insertWith (+) x 1 octoMap
















