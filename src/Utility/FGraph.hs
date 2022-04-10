{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

{- Trivially simple implementation directed graphs,
     with *integer* nodes.
-}

module Utility.FGraph where

import qualified Data.Map  as M
  -- http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html
import qualified Data.Set  as S
  -- http://www.haskell.org/ghc/docs/6.12.3/html/libraries/containers-0.3.0.0/Data-Set.html
import           Data.List (intercalate)


type Node = Integer
type FGraph = M.Map Node (S.Set Node)   -- node |-> set of neighbors


-------------------------------
-- Internal Helper Functions --
-------------------------------

missingNodes :: [Node] -> a
missingNodes nds = error ("Node(s) " ++ show nds ++ " are not in the graph")

addOneWay :: FGraph -> Node -> Node -> FGraph
addOneWay graph node1 node2 = if node1 == node2
  then graph
  else case M.lookup node1 graph of
    Just set -> M.insert node1 (S.insert node2 set) graph
    Nothing  -> M.insert node1 (S.singleton node2) graph

removeOneWay :: FGraph -> Node -> Node -> FGraph
removeOneWay graph node1 node2 = if node1 == node2
  then graph
  else case M.lookup node1 graph of
    Just set -> M.insert node1 (S.delete node2 set) graph
    Nothing  -> missingNodes [node1]

----------------------
-- Querying a Graph --
----------------------

nodes :: FGraph -> [Node]
-- Get all nodes in the graph
nodes g = M.keys g

next :: FGraph -> Node -> [Node]
-- Get the nodes pointed to by the given node.
next graph node = case M.lookup node graph of
  Nothing  -> missingNodes [node]
  Just set -> S.elems set

prev :: FGraph -> Node -> [Node]
-- Get the nodes that point to the given node.
prev g n = foldl addIfAdjacent [] (nodes g)
 where
  addIfAdjacent accum n' = if isAdjacent g n' n then n' : accum else accum


isAdjacent :: FGraph -> Node -> Node -> Bool
-- Is the given directional edge in the graph?
--    Error if either node is not in the graph.
isAdjacent g node1 node2 = case (M.lookup node1 g, M.lookup node2 g) of
  (Just s1, Just _ ) -> S.member node2 s1
  (Just _ , Nothing) -> missingNodes [node2]
  (Nothing, Just _ ) -> missingNodes [node1]
  (Nothing, Nothing) -> missingNodes [node1, node2]

-------------------------
-- Constructing Graphs --
-------------------------

empty :: FGraph
 -- empty graph
empty = M.empty

addNode :: FGraph -> Node -> FGraph
  -- Add a new node.
  --    Does nothing if the node is already in the graph.
addNode graph node =
  if M.member node graph then graph else M.insert node S.empty graph

addEdge :: FGraph -> Node -> Node -> FGraph
  -- Adds a new *directed* edge.
  --   Adds nodes to the graph as required
  --   Does nothing if the edges is already in the graph.
addEdge graph from to = addOneWay (addNode graph to) from to


removeEdge :: FGraph -> Node -> Node -> FGraph
  -- Adds a new (undirected) edge.
  --   Error if the nodes are not in the graph!
  --   Does nothing if the edges is not in the graph
  --   Does not remove nodes from the graph.
removeEdge graph from to = removeOneWay graph from to

removeNode :: FGraph -> Node -> FGraph
  -- Removes a node and all edges involving that node.
  --   Error if the node is not in the graph!
removeNode graph node = if M.member node graph
  then
    let adjs   = prev graph node
        graph' = foldl (\adj -> removeOneWay adj node) graph adjs
    in  M.delete node graph'
  else missingNodes [node]


showFGraph :: FGraph -> String
showFGraph graph = loop (M.toList graph)
 where
  loop [] = ""
  loop ((k, v) : ns) =
    (show k)
      ++ " -> "
      ++ intercalate ", " (map show (S.toList v))
      ++ "\n"
      ++ loop ns
