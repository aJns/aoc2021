import System.Environment
import Data.Char
import Data.Map (Map, member, (!))
import qualified Data.Map as Map

import Data.Ord
import Data.List

type Coord = (Int, Int)
type RiskMap = Map Coord Int
type PredMap = Map Coord Coord

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content
    let risks = map (map digitToInt) cLines

    let rows = length risks
    let cols = length $ head risks
    let keys = [[(i,j) | j <- [0..cols]] | i <- [0..rows]]
    let endNode = (rows-1, cols-1)

    -- Kokeile Dijkstran algoritmia
    -- https://fi.wikipedia.org/wiki/Dijkstran_algoritmi#Pseudokoodi

    let riskMap = Map.fromList $ concat $ map (\(k,v) -> zip k v) $ zip keys risks
    -- let path = findPath endNode [] riskMap [(0,0)]
    -- let pathRisks = map (\x -> riskMap ! x) path 
    -- let totalRisk = foldl (+) 0 pathRisks
    let startCost = Map.fromList $ [((0,0), 0)]

    putStrLn $ show $ findPathCosts startCost riskMap [(0,0)] (Map.empty) endNode


heuristic :: Coord -> Coord -> Int
heuristic (end_x, end_y) (x,y) = (end_x-x) + (end_y-y)


cost :: Coord -> RiskMap -> Coord -> Int
cost endNode riskMap node = (heuristic endNode node) + (riskMap ! node)


costWithPath :: Coord -> PredMap -> RiskMap -> RiskMap -> Coord -> Int
costWithPath endNode cameFrom riskMap costMap node = predCost + currentCost
    where currentCost = cost endNode riskMap node
          predCost = costMap ! predNode
          predNode = cameFrom ! node


findPathCosts :: RiskMap -> RiskMap -> [Coord] -> PredMap -> Coord -> RiskMap
findPathCosts costs riskMap [] _ _ = costs
findPathCosts costs riskMap openSet cameFrom endNode
  | null openSet = costs
  | node == endNode = costs
  | otherwise = findPathCosts newCosts riskMap newOpen newFrom endNode
  where node = head openSet
        newOpen = sortOn costFunction unsorted
        costFunction = costWithPath endNode cameFrom riskMap costs
        unsorted = neighbors ++ (delete node openSet)
        neighbors = findNeighbors closedSet riskMap node
        closedSet = Map.elems cameFrom
        newCosts = Map.insert node (costFunction node) costs
        newFrom = Map.union neighborFrom cameFrom
        neighborFrom = Map.fromList $ map (\x -> (node, x)) neighbors


findPath :: Coord -> [Coord] -> RiskMap -> [Coord] -> [Coord]
findPath endNode path graph openNodes
  | node == endNode = endNode:path
  | null neighbors = []
  | otherwise = findPath endNode (node:path) graph newOpen
  where neighbors = findNeighbors path graph node
        sortFunction = cost endNode graph
        node = head openNodes
        unsorted = neighbors ++ (delete node openNodes)
        newOpen = sortOn sortFunction unsorted


findNeighbors :: [Coord] -> RiskMap -> Coord -> [Coord]
findNeighbors path graph (i,j) = inGraphNotInPath
    where candidates = is ++ js
          withoutSelf = filter (\x -> not (x == (i,j))) candidates
          inGraph = filter (\x -> member x graph) withoutSelf
          inGraphNotInPath = filter (\x -> not (elem x path)) inGraph
          is = [(x, j) | x <- [i-1,i+1]]
          js = [(i, y) | y <- [j-1,j+1]]











