{-|
Module       : Color
Description  : Graph coloring, and other register-allocation tasks
Maintainer   : CS 132
-}

module Backend.Color where

import qualified Backend.X86gen  as X86gen
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import qualified Data.Set        as Set
import           IR.Temp         (Temp)
import qualified IR.Temp         as Temp
import qualified Utility.UGraph  as UGraph


-- | A coloring is a map from temporaries to registers.
--   Note that the values have type 'Temp', but we will restrict their values to
--   be just register names.
type Coloring = Map.Map Temp Temp

-- | Perform coloring, as part of register allocation.
--   Given an interference graph, a set of available register names, and a list
--   of move operations, generate a map from temporaries to hardware registers.
doColor
  :: UGraph.Graph Temp   -- ^ Interference graph
  -> Set.Set Temp        -- ^ Available hardware registers
  -> [(Temp, Temp)]      -- ^ The temp-to-temp moves in the code
                         --   (opportunities for coalescing)
  -> IO Coloring         -- ^ Output: The coloring.
                         --   (uncolored temporaries will be spilled.)

doColor igraph availableRegisters moveList = do

  -- Coalesce the graph (just once, for all moves, before we simplify)
  let (igraph', coalesced) = foldl coalesce (igraph, []) moveList

  -- Simplify the graph, getting back a stack of potentially colorable nodes
  let simplifiedStack = simplify igraph'

  -- All hardware registers must map to themselves
  let initialColoring = Map.fromList [ (r, r) | r <- X86gen.machineRegisters ]
  let coloring = select igraph' simplifiedStack initialColoring

  -- Update the coloring so that coalesced nodes share registers
  let result = foldl updateColoring coloring coalesced

  return result

  where

    k :: Int
    k = length availableRegisters

    {- True if a temporary is precolored, i.e., a register -}
    isPrecolored :: Temp -> Bool
    isPrecolored t = t `Set.member` availableRegisters

    ----------------------------------------------------------------------------
    -- Simplification (removing colorable nodes)
    ----------------------------------------------------------------------------

    {- True if degree of node is >= k -}
    isSignificant :: UGraph.Graph Temp -> Temp -> Bool
    isSignificant graph t = UGraph.neighborCount graph t >= k

    {- True if we can color this node -}
    isSimplifiable :: UGraph.Graph Temp -> Temp -> Bool
    isSimplifiable graph t = not (isPrecolored t) && not (isSignificant graph t)

    {- Simplify the graph, with optimistic spilling -}
    simplify :: UGraph.Graph Temp -> [Temp]
    simplify graph =

      let

        -- Find the nodes that are simplifiable
        simplifiable = filter (isSimplifiable graph) (UGraph.nodes graph)

        -- Remove the simplifiable nodes from the graph
        simplifiedGraph = foldl UGraph.removeNode graph simplifiable

        -- Try to remove a spill candidate
        spillCandidate = spillGraph simplifiedGraph

        in

          case spillCandidate of
            -- If we can spill a node, remove it from the graph and simplify the result
            Just t -> let graph' = UGraph.removeNode simplifiedGraph t in
                        simplify graph' ++ t : simplifiable

            -- Otherwise, we're done!
            Nothing          -> simplifiable

    ----------------------------------------------------------------------------
    -- Spilling
    ----------------------------------------------------------------------------

    {- True if we can spill this node -}
    isSpillCandidate :: UGraph.Graph Temp -> Temp -> Bool
    isSpillCandidate graph t = not (isPrecolored t) && isSignificant graph t

    {- Check whether there is a potential spill candidate -}
    spillGraph :: UGraph.Graph Temp -> Maybe Temp
    spillGraph graph  = do

      -- Find the nodes that are spill candidates
      -- If there is one, remove it from the graph
      let spillable = filter (isSpillCandidate graph) (UGraph.nodes graph)
      chooseSpillCandidate graph spillable

    {- Pick a spill candidate -}
    chooseSpillCandidate :: UGraph.Graph Temp -> [Temp] -> Maybe Temp
    chooseSpillCandidate _ []    = Nothing
    chooseSpillCandidate _ (c:_) = Just c -- TODO: smarter selection

    ----------------------------------------------------------------------------
    -- Selection (color the graph)
    ----------------------------------------------------------------------------

    {- Attempt to color the graph -}
    select :: UGraph.Graph Temp -> [Temp] -> Coloring -> Coloring

    -- If there are no temps to color, we are done!
    select _ [] coloring     = coloring

    -- Otherwise...
    select graph (t:ts) coloring =
      let
          -- Find any already-colored neighbors of t
          neighbors = UGraph.neighbors graph t
          neighborColors = Set.fromList $ mapMaybe (`Map.lookup` coloring) neighbors

          -- Find any colors that are /not/ in use by t's neighbors
          remainingColors = availableRegisters `Set.difference` neighborColors

          -- Try to choose a color
          potentialColor = chooseColor t remainingColors
          newColoring = case potentialColor of
            Nothing -> coloring
            Just c  -> Map.insert t c coloring

          in
            select graph ts newColoring

    {- Pick a color -}
    chooseColor :: Temp -> Set.Set Temp -> Maybe Temp
    chooseColor _ available =
      if null available
        then Nothing
        else Just (Set.elemAt 0 available)   -- TODO: smarter selection

    ----------------------------------------------------------------------------
    -- Coalescing (collapse the graph, in an attempt to share more registers)
    ----------------------------------------------------------------------------

    -- Given an interference graph and a move, try to coalesce two nodes
    -- Return a possibly updated graph and possibly a pair of temporaries
    --  (the node kept, and the node removed)
    doCoalesce :: UGraph.Graph Temp -> (Temp, Temp) -> (UGraph.Graph Temp, Maybe (Temp, Temp))
    doCoalesce graph pair =
      if isCoalescable graph pair
        then (coalesceNodes graph (chooseCoalesce pair), Just (chooseCoalesce pair))
        else (graph, Nothing)

    -- Given an interference graph and a pair of nodes, return true if the nodes can
    -- be coalesced in the graph.
    isCoalescable :: UGraph.Graph Temp -> (Temp, Temp) -> Bool
    isCoalescable graph (n1, n2) =
         n1 /= n2
      && UGraph.hasNode graph n1 && UGraph.hasNode graph n2
      && not (interferesWith graph n1 n2)
      && george graph (n1, n2)

    -- The "George" condition for coalescing
    george :: UGraph.Graph Temp -> (Temp, Temp) -> Bool
    george graph (n1, n2) =
      all (\n -> interferesWith graph n n2 || UGraph.neighborCount graph n < k) (UGraph.neighbors graph n1)

    -- Given a pair of nodes that can be coalesced, choose
    -- which one to keep and which one to remove
    chooseCoalesce :: (Temp, Temp) -> (Temp, Temp)
    chooseCoalesce (n1, n2) = if Temp.isRegister n1 then (n1, n2) else (n2, n1)

    -- Given an interference graph and a pair of nodes, coalesce the nodes in the graph
    coalesceNodes :: UGraph.Graph Temp -> (Temp, Temp) -> UGraph.Graph Temp
    coalesceNodes graph (kept, removed) =
      let neighbors = UGraph.neighbors graph removed in
        foldl (\ g n -> UGraph.addEdge g n kept) (UGraph.removeNode graph removed) neighbors

    -- A function to potentially coalesce a single pair of nodes
    coalesce (g, rs) p = case doCoalesce g p of
      (_, Nothing) -> (g, rs)
      (g', Just r) -> (g', r:rs)

    -- A function to update the coloring, so that a coalesced pair shares a register
    updateColoring coloring (kept, removed) =
      case Map.lookup kept coloring of
        Nothing    -> coloring
        Just color -> Map.insert removed color coloring



interferesWith :: UGraph.Graph Temp -> Temp -> Temp -> Bool
interferesWith = UGraph.isAdjacent
