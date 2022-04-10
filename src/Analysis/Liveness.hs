{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

{- Known issues:
     The resulting graph will include machine registers if temporaries
          interfere with them, but it does *not* necessarily include all
          machine registers, and does not include interference between
          machine registers.

     If your code has a bug where a temporary is used without being
          defined first, it will be live at the program entry.
          All such variables should interfere with each other; this
          code does not ensure that, however.
-}

module Analysis.Liveness
  ( buildIGraph
  , MoveList
  )
where

import qualified Analysis.Makegraph as Makegraph
import qualified Backend.X86gen     as X86gen
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import           IR.Temp            (Temp)
import qualified Utility.FGraph     as F
import qualified Utility.UGraph     as U

type LiveMap = M.Map F.Node (S.Set Temp)

find :: LiveMap -> F.Node -> S.Set Temp
find mp n = case M.lookup n mp of
  Nothing -> error ("Missing node " ++ (show n))
  Just x  -> x

type MoveList = [(Temp, Temp)]

buildIGraph :: Makegraph.FlowInfo -> (LiveMap, U.Graph Temp, MoveList)
buildIGraph (g0, defmap, usemap, moveset) =

  let

    nodes = F.nodes g0

    noLive :: LiveMap
    noLive = foldl (\mp nd -> M.insert nd S.empty mp) M.empty nodes

    pass [] livein liveout changed = (livein, liveout, changed)
    pass (n : ns) livein liveout changed =
      let
        in_set  = find livein n
        out_set = find liveout n

        in_set' =
          S.union (find usemap n) (S.difference out_set (find defmap n))
        out_set' = S.unions (map (find livein) (F.next g0 n))

        changed' =
          changed
            || (S.size in_set /= S.size in_set')
            || (S.size out_set /= S.size out_set')

        livein'  = M.insert n in_set' livein
        liveout' = M.insert n out_set' liveout
      in
        pass ns livein' liveout' changed'


    iterateToFixedPoint livein liveout =
      let (livein', liveout', changed') = pass nodes livein liveout False
      in  if changed'
            then iterateToFixedPoint livein' liveout'
            else (livein', liveout')

    (finalLivein, finalLiveout) = iterateToFixedPoint noLive noLive


   -- Get a list of all relevant temporaries
   --    (mentioned, or machine registers)
    temps                       = S.toList (gettemps nodes)
     where
      gettemps []       = S.fromList X86gen.machineRegisters
      gettemps (n : ns) = S.unions [gettemps ns, find usemap n, find defmap n]

    -- Find all the move instructions
    moves = moveloop nodes
     where
      moveloop []       = []
      moveloop (n : ns) = if (S.member n moveset)
        then (head (S.toList (find usemap n)), head (S.toList (find defmap n)))
          : moveloop ns
        else moveloop ns

    {- OK, now we put the edges in the adjacency graph -}
    buildigraph :: [F.Node] -> U.Graph Temp
    buildigraph [] =
      let discreteGraph :: U.Graph Temp
           -- A Graph with a node for each temporary.
          discreteGraph = foldl (\g t -> U.addNode g t) U.empty temps

          finalGraph :: U.Graph Temp
          -- discreteGraph + interference between all distinct machine registers
          finalGraph = foldl
            (\g (r1, r2) -> U.addEdge g r1 r2)
            discreteGraph
            [ (r1, r2)
            | r1 <- X86gen.machineRegisters
            , r2 <- X86gen.machineRegisters
            , r1 < r2
            ]
      in  finalGraph
    buildigraph (n : ns) =
      let g    = buildigraph ns
          lout = S.toList (find finalLiveout n)
          def  = S.toList (find defmap n)
          use  = S.toList (find usemap n)
      in  case (S.member n moveset, def, use) of
            (True, [d], [u]) -> foldl addMoveEdge g lout
             where
              addMoveEdge gr t' = if t' /= u then U.addEdge gr d t' else gr
            _ -> foldl addEdge g [ (x, y) | x <- def, y <- lout ]
              where addEdge gr (x, y) = U.addEdge gr x y
  in
    (finalLivein, buildigraph nodes, moves)
