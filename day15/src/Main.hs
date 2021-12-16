import System.Environment
import Data.Ord
import Data.List
import Data.Char
import Data.Map (Map, member, (!))
import qualified Data.Map as Map

type Coord = (Int, Int)
type RiskMap = Map Coord Int

type Node = Coord
type Edge = (Node, Int)
type Graph = Map Node [Edge]
type DistanceMap = Map Node Int

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
    let graph = buildGraph Map.empty riskMap (Map.keys riskMap)

    putStrLn $ show $ dijkstra graph (0,0)


buildGraph :: Graph -> RiskMap -> [Node] -> Graph
buildGraph graph riskMap [] = graph
buildGraph graph riskMap (node:nodes) =
  let newGraph = Map.insert node edges graph
      edges = map (\x -> (x, (riskMap ! x))) neighbors
      neighbors = filter (\x -> elem x (Map.keys riskMap)) unfiltered
      unfiltered = row ++ col
      row = [(i, y) | y <- [j-1, j+1]]
      col = [(x, j) | x <- [i-1, i+1]]
      i = fst node
      j = snd node
  in buildGraph newGraph riskMap nodes



dijkstra :: Graph -> Node -> DistanceMap
dijkstra graph startNode = dijkstraLoop graph distances openSet
  where distances = Map.insert startNode 0 $ Map.fromList maxDistances
        maxDistances = map (\(x,y) -> ((x,y), (x*9)+(y*9))) $ Map.keys graph
        openSet = Map.keys graph


dijkstraLoop :: Graph -> DistanceMap -> [Node] -> DistanceMap
dijkstraLoop graph distances [] = distances
dijkstraLoop graph distances openSet = dijkstraLoop graph newDist newOpen
  where (node, newOpen) = extractMin distances openSet
        newDist = Map.unionWith (min) neighborDistMap distances
        neighborDist = map (\(neighbor, dist) -> (neighbor, (distances ! node) + dist)) $ graph ! node
        neighborDistMap = Map.fromList neighborDist


extractMin :: DistanceMap -> [Node] -> (Node, [Node])
extractMin distances openSet = (node, newOpen)
  where node = head sorted
        newOpen = drop 1 sorted
        sorted = sortBy (\x y -> compare (distances ! x) (distances ! y)) openSet








