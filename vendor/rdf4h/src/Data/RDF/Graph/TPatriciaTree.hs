{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}

module Data.RDF.Graph.TPatriciaTree (TPatriciaTree) where

import Data.RDF.Namespace
import Data.RDF.Query
import Data.RDF.Types

import Control.DeepSeq (NFData)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import GHC.Generics
import Data.Binary (Binary)

data TPatriciaTree deriving (Generic)
instance Binary TPatriciaTree
instance NFData TPatriciaTree

data instance RDF TPatriciaTree = TPatriciaTree (PT.Gr Node Node,IntMap.IntMap Node, Maybe BaseUrl, PrefixMappings)
                                deriving (NFData,Generic)

instance Rdf TPatriciaTree where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = triplesOf'
  uniqTriplesOf     = uniqTriplesOf'
  select            = select'
  query             = query'
  showGraph         = show . triplesOf'

empty' :: RDF TPatriciaTree
empty' = TPatriciaTree (G.empty,IntMap.empty, Nothing, PrefixMappings Map.empty)

prefixMappings' :: RDF TPatriciaTree -> PrefixMappings
prefixMappings' (TPatriciaTree (_,_,_,pms')) = pms'

addPrefixMappings' :: RDF TPatriciaTree -> PrefixMappings -> Bool -> RDF TPatriciaTree
addPrefixMappings' (TPatriciaTree (g, idxLookup, baseURL, pms)) pms' replace =
  let merge = if replace then flip (<>) else (<>)
  in  TPatriciaTree (g, idxLookup, baseURL, merge pms pms')

baseUrl' :: RDF TPatriciaTree -> Maybe BaseUrl
baseUrl' (TPatriciaTree _) = Nothing

data AutoIncrMap = AutoIncrMap
    { theMap :: Map.Map Node (Int,Node)
    , idxPtr :: !Int }

{-
init       -> []
insert www -> [0,www]
insert ttt -> [1,ttt]
insert ttt -> [1,ttt]
-}
insertIncr :: Node -> AutoIncrMap -> (Int,AutoIncrMap)
insertIncr !node mp =
    let x = Map.lookup node (theMap mp)
    in if isJust x
       then
           let (i,_) = fromJust x
           in (i,mp)
       else
           let curIdx = idxPtr mp
               mp' = mp { idxPtr = curIdx + 1
                        , theMap = Map.insert node (idxPtr mp, node) (theMap mp) }
           in (curIdx, mp')

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TPatriciaTree
mkRdf' ts base' pms' =
    -- step 1: subjects and objects assigned node ID (an Int)
    let (mp,ledges) = foldl' f ((AutoIncrMap Map.empty 0),[]) ts

        -- step 2: for all triples, create arc with
        --         - predicate node the arc label
        --         - subject the source node ID
        --         - object the target node ID
        f (mp',edges) (Triple s p o) =
            let (sIdx,mp'')  = insertIncr s mp'
                (oIdx,mp''') = insertIncr o mp''
                edge = (sIdx,oIdx,p)
            in (mp''',edge : edges)

        lnodes = Map.elems (theMap mp)
        intIdx = IntMap.fromList lnodes
        ptGraph = G.mkGraph lnodes ledges

    in TPatriciaTree (ptGraph ,intIdx, base', pms')

triplesOf' :: RDF TPatriciaTree -> Triples
triplesOf' (TPatriciaTree (g,idxLookup,_,_)) =
    fmap (\(sIdx,oIdx,p) ->
             let [s,o] = fmap (\idx -> fromJust $ IntMap.lookup idx idxLookup) [sIdx,oIdx]
             in Triple s p o) (G.labEdges g)

uniqTriplesOf' :: RDF TPatriciaTree -> Triples
uniqTriplesOf' ptG@(TPatriciaTree (g,idxLookup,_,_)) =
    nub $ fmap (\(sIdx,oIdx,p) ->
             let [s,o] = fmap (\idx -> fromJust $ IntMap.lookup idx idxLookup) [sIdx,oIdx]
             in expandTriple (prefixMappings ptG) (Triple s p o)) (G.labEdges g)

mkTriples :: IntMap.IntMap Node -> Node -> [(Node, IntMap.Key)] -> [(Node, IntMap.Key)] ->  [Triple]
mkTriples idxLookup thisNode adjsIn adjsOut =
    let ts1 = fmap (\(predNode,subjIdx) ->
                   let s = fromJust (IntMap.lookup subjIdx idxLookup)
                   in Triple s predNode thisNode
                  )  adjsIn

        ts2 = fmap (\(predNode,objIdx) ->
                       let o = fromJust (IntMap.lookup objIdx idxLookup)
                       in Triple thisNode predNode o
                  ) adjsOut
    in ts1 <> ts2

select' :: RDF TPatriciaTree -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (TPatriciaTree (g,idxLookup,_,_)) maybeSubjSel maybePredSel maybeObjSel =

    let cfun ( adjsIn , _nodeIdx , thisNode , adjsOut )
            | isNothing  maybeSubjSel && isNothing maybePredSel && isNothing maybeObjSel =
                         mkTriples idxLookup thisNode adjsIn adjsOut

            | isJust maybeSubjSel && isNothing maybePredSel && isNothing maybeObjSel =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup))) adjsIn
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut
                                 else []
                       in ts1 <> ts2
            | isNothing maybeSubjSel && isJust maybePredSel && isNothing maybeObjSel =
                       let adjsIn'  = filter (\(p,_idxSubj) -> fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> fromJust maybePredSel p ) adjsOut
                           ts1 = if not (null adjsIn')
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if not (null adjsOut')
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

            | isNothing maybeSubjSel && isNothing maybePredSel && isJust maybeObjSel =
                       let adjsOut' = filter (\(_p,idxObj) -> fromJust maybeObjSel (fromJust (IntMap.lookup idxObj idxLookup)) ) adjsOut
                           ts1 = mkTriples idxLookup thisNode [] adjsOut'
                           ts2 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn []
                                 else []
                       in ts1 <> ts2

            | isJust maybeSubjSel && isJust maybePredSel && isNothing maybeObjSel =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup))
                                                            && fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> fromJust maybePredSel p ) adjsOut
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

            | isJust maybeSubjSel && isNothing maybePredSel && isJust maybeObjSel =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup)) ) adjsIn
                           adjsOut' = filter (\(_p,idxObj) -> fromJust maybeObjSel  (fromJust (IntMap.lookup idxObj idxLookup))  ) adjsOut
                           ts1 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

            | isNothing maybeSubjSel && isJust maybePredSel && isJust maybeObjSel =
                       let adjsIn' = filter (\(p,_idxSubj) -> fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust maybeObjSel (fromJust (IntMap.lookup idxObj idxLookup))
                                                            && fromJust maybePredSel p ) adjsOut
                           ts1 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = mkTriples idxLookup thisNode [] adjsOut'
                       in ts1 <> ts2

            | isJust maybeSubjSel && isJust maybePredSel && isJust maybeObjSel =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup))
                                                            && fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust maybeObjSel (fromJust (IntMap.lookup idxObj idxLookup))
                                                            && fromJust maybePredSel p ) adjsOut
                           ts1 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

        cfun ( _ , _ , _ , _) = undefined -- not sure why this pattern is needed to exhaust cfun arg patterns

    in concat $ DFS.dfsWith' cfun g

query' :: RDF TPatriciaTree -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' (TPatriciaTree (g,idxLookup,_,_)) maybeSubj maybePred maybeObj =

    let cfun ( adjsIn , _nodeIdx , thisNode , adjsOut )
            | isNothing  maybeSubj && isNothing maybePred && isNothing maybeObj =
                       mkTriples idxLookup thisNode adjsIn adjsOut

            | isJust maybeSubj && isNothing maybePred && isNothing maybeObj =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj ) adjsIn
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if thisNode == fromJust maybeSubj
                                 then mkTriples idxLookup thisNode [] adjsOut
                                 else []
                       in ts1 <> ts2

            | isNothing maybeSubj && isJust maybePred && isNothing maybeObj =
                       let adjsIn'  = filter (\(p,_idxSubj) -> p == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> p  == fromJust maybePred ) adjsOut
                           ts1 = if not (null adjsIn')
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if not (null adjsOut')
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

            | isNothing maybeSubj && isNothing maybePred && isJust maybeObj =
                       let adjsOut' = filter (\(_p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj ) adjsOut
                           ts1 = mkTriples idxLookup thisNode [] adjsOut'
                           ts2 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn []
                                 else []
                       in ts1 <> ts2

            | isJust maybeSubj && isJust maybePred && isNothing maybeObj =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj
                                                            && p  == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> p  == fromJust maybePred ) adjsOut
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if thisNode == fromJust maybeSubj
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

            | isJust maybeSubj && isNothing maybePred && isJust maybeObj =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj ) adjsIn
                           adjsOut' = filter (\(_p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj ) adjsOut
                           ts1 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if thisNode == fromJust maybeSubj
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

            | isNothing maybeSubj && isJust maybePred && isJust maybeObj =
                       let adjsIn' = filter (\(p,_idxSubj) -> p  == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj
                                                            && p  == fromJust maybePred ) adjsOut
                           ts1 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = mkTriples idxLookup thisNode [] adjsOut'
                       in ts1 <> ts2

            | isJust maybeSubj && isJust maybePred && isJust maybeObj =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj
                                                            && p  == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj
                                                            && p  == fromJust maybePred ) adjsOut
                           ts1 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if thisNode == fromJust maybeSubj
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 <> ts2

        cfun ( _ , _ , _ , _ ) = undefined  -- not sure why this pattern is needed to exhaust cfun arg patterns

    in concat $ DFS.dfsWith' cfun g
