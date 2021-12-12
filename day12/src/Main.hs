import System.Environment
import Data.Strings
import Data.Char

import Data.Map (Map, insertWith, empty, delete, (!), insert, member, elems)
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
    where newGraph = insertWith (++) node [neighbor] oneWay
          oneWay = insertWith (++) neighbor [node] graph
          (node, neighbor) = strSplit "-" x


exploreNeighbors :: Graph -> [String] -> String -> [[String]]
exploreNeighbors graph path node 
  | node == "end" = [newPath]
  | not (canExplore path node) = []
  | not (member node graph) = []
  | null neighbors = []
  | otherwise = concat $ map (exploreNeighbors graph newPath) neighbors
  where newPath = node:path
        neighbors = graph ! node


canExplore :: [String] -> String -> Bool
canExplore path node
  | node == "start" = occur < 1
  | isUpper (head node) = True
  | otherwise = if not $ hasDoubleLower path
                   then occur < 2
                   else occur < 1
  where occur = length $ filter (node==) path


hasDoubleLower :: [String] -> Bool
hasDoubleLower path = hasDoubleLower' empty path


hasDoubleLower' :: Map String Int -> [String] -> Bool
hasDoubleLower' occurMap [] = hasDouble
    where hasDouble = (length filtList) > 0
          filtList = filter (\x -> x > 1) (elems occurMap) 
hasDoubleLower' occurMap (x:xs) = hasDoubleLower' newMap xs
    where newMap = if isLower (head x) then insertWith (+) x 1 occurMap
                                       else occurMap












