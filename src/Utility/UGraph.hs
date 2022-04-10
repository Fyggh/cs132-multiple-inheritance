{-|
Module       : UGraph
Description  : Trivially simple, functional implementation of undirected graphs
Maintainer   : CS 132

Because the implementation is functional, adding or removing edges produces
a new graph, and does not modify the existing graph.

No self-loops are allowed. (If you try to add an edge from a node to
itself, the graph remains unchanged.)
-}

module Utility.UGraph (
  Graph

  -- * Querying a graph
  , nodes
  , neighbors
  , neighborCount
  , isAdjacent
  , hasNode

  -- * Constructing graphs
  , empty
  , addNode
  , addEdge
  , removeEdge
  , removeNode
  , map

)
where

import           Data.List (intercalate)
import qualified Data.Map  as M
import qualified Data.Set  as S
import           Prelude   hiding (map)
import qualified Prelude   as P

-- | Maps nodes to sets of neighbors
newtype Graph a = IG (M.Map a (S.Set a))

-----------------------------------------------------------------------------------------
-- Querying a graph
-----------------------------------------------------------------------------------------

-- | Get all nodes in the graph
nodes :: Graph a -> [a]
nodes (IG g) = M.keys g

-- | Get all neighbors of the given node /as a list/
neighbors :: (Ord a, Show a) => Graph a -> a -> [a]
neighbors (IG graph) node = case M.lookup node graph of
  Nothing  -> missingNodes "neighbors" [node]
  Just set -> S.elems set

-- | Count the neighbors of a node
neighborCount :: (Ord a, Show a) => Graph a -> a -> Int
neighborCount (IG graph) node = case M.lookup node graph of
  Nothing  -> missingNodes "neighborCount" [node]
  Just set -> S.size set

-- | Are the two nodes adjacent in the graph?
isAdjacent :: (Ord a, Show a) => Graph a -> a -> a -> Bool
isAdjacent (IG g) node1 node2 = case (M.lookup node1 g, M.lookup node2 g) of
  (Just s1, Just _ ) -> S.member node2 s1
  (Just _ , Nothing) -> missingNodes "isAdjacent" [node2]
  (Nothing, Just _ ) -> missingNodes "isAdjacent" [node1]
  (Nothing, Nothing) -> missingNodes "isAdjacent" [node1, node2]

-- | Is a node in the graph?
hasNode :: Ord a => Graph a -> a -> Bool
hasNode (IG g) node = M.member node g

-----------------------------------------------------------------------------------------
-- Constructing graphs
-----------------------------------------------------------------------------------------

-- | The empty graph
empty :: Graph a
empty = IG M.empty

-- | Add a new node.
--   Does nothing if the node is already in the graph.
addNode :: (Ord a, Show a) => Graph a -> a -> Graph a
addNode (IG graph) node =
  if M.member node graph then IG graph else IG (M.insert node S.empty graph)

-- | Adds a new (undirected) edge.
--   Adds nodes to the graph as required
--   Does nothing if the edges is already in the graph.
addEdge :: (Ord a, Show a) => Graph a -> a -> a -> Graph a
addEdge graph from to = addOneWay (addOneWay graph from to) to from

-- | Adds a new (undirected) edge.
--   Error if the nodes are not in the graph!
--   Does nothing if the edges is not in the graph
--   Does not remove nodes from the graph.
removeEdge :: (Ord a, Show a) => Graph a -> a -> a -> Graph a
removeEdge graph from to = removeOneWay (removeOneWay graph from to) to from

-- | Removes a node and all edges involving that node.
--   Error if the node is not in the graph!
removeNode :: (Ord a, Show a) => Graph a -> a -> Graph a
removeNode (IG graph) node = if M.member node graph
  then
    let adjs = neighbors (IG graph) node
        IG graph' =
          foldl (\grf adj -> removeOneWay grf adj node) (IG graph) adjs
    in  IG (M.delete node graph')
  else missingNodes "removeNode" [node]

-- | Use an a->b function to turn a graph with nodes of type a's
--   into a graph with nodes of type b's
map :: (Ord a, Ord b) => (a -> b) -> Graph a -> Graph b
map f (IG graph) = IG $ M.map (S.map f) $ M.mapKeys f graph

-----------------------------------------------------------------------------------------
-- Displaying graphs
-----------------------------------------------------------------------------------------

instance (Show a) => Show (Graph a) where
  show (IG g) = loop (M.toList g)
   where
    loop [] = ""
    loop ((k, v) : ns) =
      show k
        ++ " : "
        ++ intercalate ", " (P.map show (S.toList v))
        ++ "\n"
        ++ loop ns


-----------------------------------------------------------------------------------------
-- Internal helper functions
-----------------------------------------------------------------------------------------

missingNodes :: Show a => String -> [a] -> b
missingNodes msg nds =
  error ("Nodes " ++ show nds ++ " are not in the graph [" ++ msg ++ "]")

addOneWay :: (Ord a, Show a) => Graph a -> a -> a -> Graph a
addOneWay (IG graph) node1 node2 =
  if node1 == node2
   then IG graph
   else case M.lookup node1 graph of
     Just set -> IG (M.insert node1 (S.insert node2 set) graph)
     Nothing  -> IG (M.insert node1 (S.singleton node2) graph)

removeOneWay :: (Ord a, Show a) => Graph a -> a -> a -> Graph a
removeOneWay (IG graph) node1 node2 =
  if node1 == node2
    then IG graph
    else case M.lookup node1 graph of
      Just set -> IG (M.insert node1 (S.delete node2 set) graph)
      Nothing  -> missingNodes "removeOneWay" [node1]
