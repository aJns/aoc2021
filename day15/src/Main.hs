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

    let incrementer = [0..4]

    let colRisks = map (incrementRow incrementer) risks
    let bigRisks = transpose $ map (incrementRow incrementer) $ transpose colRisks

    let rows = length bigRisks
    let cols = length $ head bigRisks
    let keys = [[(i,j) | j <- [0..cols]] | i <- [0..rows]]
    let endNode = (rows-1, cols-1)

    let riskMap = Map.fromList $ concat $ map (\(k,v) -> zip k v) $ zip keys bigRisks
    let graph = buildGraph Map.empty riskMap (Map.keys riskMap)
    let costMap = dijkstra graph ((0,0), endNode)
    let path = reconstructPath graph costMap endNode []
    let pathCost = show $ foldl (+) 0 $ map (\x -> riskMap ! x) $ drop 1 path

    let stringRepr = intercalate "\n" $ map (map (\x -> intToDigit x)) bigRisks

    putStrLn $ pathCost



incrementWithWrap :: Int -> Int -> Int
incrementWithWrap a b
  | c > 9 = c - 9
  | otherwise = c
  where c = a + b


incrementRow :: [Int] -> [Int] -> [Int]
incrementRow cols row = concat $ map (\x -> map (incrementWithWrap x) row) cols


reconstructPath :: Graph -> DistanceMap -> Node-> [Node] -> [Node]
reconstructPath graph distances node path
  | (distances ! node) == 0 = newPath
  | otherwise = reconstructPath graph distances newNode newPath
  where newPath = node:path
        newNode = head sorted
        sorted = sortBy (\x y -> compare (distances ! x) (distances ! y)) neighbors
        neighbors = map (\(x,y) -> x) $ graph ! node



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



dijkstra :: Graph -> (Node, Node) -> DistanceMap
dijkstra graph (startNode, endNode) = dijkstraLoop graph endNode distances openSet
  where distances = Map.insert startNode 0 $ Map.fromList maxDistances
        maxDistances = map (\(x,y) -> ((x,y), (x*9)+(y*9))) $ Map.keys graph
        openSet = Map.keys graph


dijkstraLoop :: Graph -> Node -> DistanceMap -> [Node] -> DistanceMap
dijkstraLoop _ _ distances [] = distances
dijkstraLoop graph endNode distances openSet
  | node == endNode = newDist
  | otherwise = dijkstraLoop graph endNode newDist newOpen
  where (node, newOpen) = extractMin distances openSet
        newDist = Map.unionWith (min) neighborDistMap distances
        neighborDist = map (\(neighbor, dist) -> (neighbor, (distances ! node) + dist)) $ graph ! node
        neighborDistMap = Map.fromList neighborDist


extractMin :: DistanceMap -> [Node] -> (Node, [Node])
extractMin distances openSet = (node, newOpen)
  where node = head sorted
        newOpen = drop 1 sorted
        sorted = sortBy (\x y -> compare (distances ! x) (distances ! y)) openSet








