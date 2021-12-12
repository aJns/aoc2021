import System.Environment
import Data.Strings
import Data.Char

import Data.Map (Map, insertWith, empty, delete, (!), insert, member)
import qualified Data.Map as Map

-- Node, neighbors
type Graph = Map String [String]

main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let cLines = lines content

    let caveGraph = buildGraph empty cLines

    putStrLn $ show $ length $ exploreNeighbors caveGraph [] "start"


buildGraph :: Graph -> [String] -> Graph
buildGraph graph [] = graph
buildGraph graph (x:xs) = buildGraph newGraph xs
    where newGraph = insertWith (++) node [neighbor] graph
          (node, neighbor) = strSplit "-" x


exploreNeighbors :: Graph -> [String] -> String -> [[String]]
exploreNeighbors graph path node 
  | not (canExplore path node) = []
  | not (member node graph) = [newPath]
  | node == "end" = [newPath]
  | null neighbors = [newPath]
  | otherwise = concat $ map (exploreNeighbors graph newPath) neighbors
  where newPath = node:path
        neighbors = graph ! node


canExplore :: [String] -> String -> Bool
canExplore path node = occur <= allowed
  where occur = length $ filter (node==) path
        allowed = if isLower (head node) then 1 else 2
