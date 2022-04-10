{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module Analysis.Makegraph where

import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified IR.Assem       as A
import qualified IR.Temp        as Temp

import qualified Utility.FGraph as FGraph

type LabelMap = M.Map Temp.Label FGraph.Node

type DefMap = M.Map FGraph.Node (S.Set Temp.Temp)
type UseMap = M.Map FGraph.Node (S.Set Temp.Temp)
type MoveSet = S.Set FGraph.Node

type FlowInfo = (FGraph.FGraph, DefMap, UseMap, MoveSet)

instrs2graph :: [A.Instr] -> FlowInfo
instrs2graph instrs =
  let ns       = makeNodes 0 instrs
      labelMap = rememberLabels instrs ns
  in  makeGraph labelMap instrs ns

makeNodes :: FGraph.Node -> [a] -> [FGraph.Node]
makeNodes _ []         = []
makeNodes n (_ : rest) = n : makeNodes (n + 1) rest


rememberLabels :: [A.Instr] -> [FGraph.Node] -> LabelMap
rememberLabels [] [] = M.empty
rememberLabels (A.LABEL _ l : instrs) (n : ns) =
  M.insert l n (rememberLabels instrs ns)
rememberLabels (_ : instrs) (_ : ns) = rememberLabels instrs ns
rememberLabels _ _ =
  error "rememberLabels: different number of nodes and labels"


makeGraph :: LabelMap -> [A.Instr] -> [FGraph.Node] -> FlowInfo
makeGraph _ [] [] = (FGraph.empty, M.empty, M.empty, S.empty)
makeGraph lm (instr : instrs) (n : ns) =
  let (g, defmap, usemap, moveset) = makeGraph lm instrs ns
  in
    case instr of
      A.OPER _ src dst Nothing ->
        let g' = case ns of
              n' : _ -> FGraph.addEdge g n n'
              _      -> FGraph.addNode g n
            defmap'  = M.insert n (S.fromList dst) defmap
            usemap'  = M.insert n (S.fromList src) usemap
            moveset' = moveset
        in  (g', defmap', usemap', moveset')

      A.MOVE _ src dst ->
        let g' = case ns of
              n' : _ -> FGraph.addEdge g n n'
              _      -> FGraph.addNode g n
            defmap'  = M.insert n (S.singleton dst) defmap
            usemap'  = M.insert n (S.singleton src) usemap
            moveset' = S.insert n moveset
        in  (g', defmap', usemap', moveset')

      A.OPER asm src dst (Just ls) ->
        let
          nextEdge gr l = case M.lookup l lm of
            Just n' -> FGraph.addEdge gr n n'
            Nothing ->
              error ("Instruction " ++ asm ++ " jumps to undefined label " ++ l)
          g'       = foldl nextEdge g ls
          defmap'  = M.insert n (S.fromList dst) defmap
          usemap'  = M.insert n (S.fromList src) usemap
          moveset' = moveset
        in
          (g', defmap', usemap', moveset')

      A.LABEL _ _ ->
        let g' = case ns of
              n' : _ -> FGraph.addEdge g n n'
              _      -> FGraph.addNode g n
            defmap'  = M.insert n S.empty defmap
            usemap'  = M.insert n S.empty usemap
            moveset' = moveset
        in  (g', defmap', usemap', moveset')

      A.RETURN _ src ->
        let g'       = FGraph.addNode g n
            defmap'  = M.insert n S.empty defmap
            usemap'  = M.insert n (S.fromList src) usemap
            moveset' = moveset
        in  (g', defmap', usemap', moveset')

makeGraph _ _ _ = error "makeGraph: different number of nodes and labels"
