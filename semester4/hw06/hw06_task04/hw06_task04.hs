module Main where

import Data.List
import Data.Maybe
import Data.Tuple
import Control.Monad

type VertexId = Int
type Distance = Double

data Graph = Graph { vertices :: [VertexId]
                   , edges    :: [((VertexId, VertexId), Distance)]
                   }
    deriving (Show)

data Path = Path { idx  :: VertexId
                 , dist :: Distance
                 , prev :: VertexId
                 }
    deriving (Show, Eq)

instance Ord Path where
    compare x y = dist x `compare` dist y

infinity :: Distance
infinity = 1.0 / 0.0

dijkstra :: Graph -> VertexId -> [Path]
dijkstra (Graph vs es) s = process (map (\v -> Path v (if v == s then 0.0 else infinity) v) vs) []
    where process :: [Path] -> [Path] -> [Path]
          process [] visited = visited
          process unvisited visited =
              let nearestVertex = minimum unvisited
                  newVisited = nearestVertex : visited
                  newUnvisited = (map (\p -> min p (makePathThrough nearestVertex p)) . delete nearestVertex) unvisited
                  makePathThrough (Path nrstId nrstD _) (Path pId _ _) = Path pId (nrstD + distBetween nrstId pId) nrstId
              in  process newUnvisited newVisited
          distBetween id1 id2 = fromJust $ lookup (id1, id2) es `mplus` Just infinity

makeUnordered :: Graph -> Graph
makeUnordered (Graph vs es) = Graph vs $ foldr (\e@(uv, d) -> ([e, (swap uv, d)] ++)) [] es

testGraph :: Graph
testGraph = Graph { vertices = [1..6]
                  , edges = [((1, 2), 2.0)
                           , ((3, 2), 3.0)
                           , ((4, 3), 7.0)
                           , ((3, 6), 17.0)
                           , ((1, 4), 15.0)
                           , ((1, 3), 6.0)
                           , ((4, 6), 8.0)
                           ]
                  }

main = do
    putStrLn $ "The test graph: " ++ show testGraph ++
               "\nThe shortest paths from vertex 1: " ++ show (dijkstra (makeUnordered testGraph) 1)
