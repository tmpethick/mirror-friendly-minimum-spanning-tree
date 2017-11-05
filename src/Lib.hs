{-# LANGUAGE FlexibleContexts #-}

module Lib where

  import Data.Tuple.Select
  import Data.Graph.Inductive.Basic
  import Data.List  
  import Data.Graph.Inductive.Graph
  import Data.Graph.Inductive.PatriciaTree

  nodes' :: [LNode Int]
  nodes' = zip [1..6] [1..6]
  
  edges' :: [LEdge Int]
  edges' = [
    (1,2,1),
    (1,3,2),
    (1,4,3),
    (1,6,4),
    (2,3,5),
    (2,5,6),
    (3,4,7),
    (4,5,8)]

  g :: Gr Int Int
  g = undir $ mkGraph nodes' edges'

  hasOneEdge :: DynGraph gr => gr a b -> Node -> Bool
  hasOneEdge g = (== 1) . length . out g
  
  powerset :: [a] -> [[a]]
  powerset [] = [[]]
  powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs
  
  powersetWithoutEmptySet = init . powerset

  root = head . nodes
  n = root g
  
  getPendant :: DynGraph gr => gr a b -> Node -> [Node]
  getPendant g n = filter (hasOneEdge g) (fmap sel2 (out g n))
  
  removePendant :: (DynGraph gr, Eq b) => gr a b -> [Node] -> gr a b
  removePendant g ns = foldr delAllLEdge g $ fmap (head . out g) ns ++ fmap (head . inn g) ns
  
  primaryPartitions g n = powersetWithoutEmptySet $ out g n
  
  type PartitionPair a = (Partition a, Partition a)
  data Partition a = EdgePart (LEdge a) | NodePart Node

  delPrimaryPartition :: (Sel2 a Node, Sel1 a Node, Graph gr) => gr a1 b -> [a] -> gr a1 b
  delPrimaryPartition g pp = delNodes (fmap sel1 pp ++ fmap sel2 pp) g

  getPartition :: Gr a a1 -> Partition a1
  getPartition pg = case out pg n of 
      []    -> NodePart n
      (h:t) -> EdgePart h
    where n = root pg
  
  removePartition :: Graph gr => Partition t -> gr a b -> gr a b
  removePartition (EdgePart e) g = delPrimaryPartition g [e]
  removePartition (NodePart n) g = delNode n g

  getSecondaryPartitions :: Gr a t -> [Partition t]
  getSecondaryPartitions g 
    | isEmpty g = []
    | otherwise = p : getSecondaryPartitions (removePartition p g)
      where p = getPartition g

  -- TODO: includes single left-out vertices?
  pairNeighbors :: [b] -> [(b, b)]
  pairNeighbors l = zip l (tail l)
  
  toNodes :: Partition t -> [Node]
  toNodes (EdgePart (n1, n2, _)) = [n1, n2]
  toNodes (NodePart n) = [n]

  partitionCombinations :: PartitionPair a -> [Edge]
  partitionCombinations (p1, p2) = (,) <$> toNodes p1 <*> toNodes p2

  connectorsForPair :: PartitionPair a -> [Edge]
  connectorsForPair parts = filter (hasEdge g) (partitionCombinations parts)
  
  -- nps = pairNeighbors sps
  -- connectors nps = fmap connectorsForPair nps
  -- connectPartitions nps = foldr nps

  -- flattenTuple tl = concat [[a,b] | (a,b) <- tl]

  -- subgraph (flattenTuple (connectorsForPair parts)) g

  -- divide
  -- conquer
  -- combine
  --  dcc
  --   | trivial y = solve y
  --   | otherwise = (combine . map conquer . divide) y

-- map all spanning trees to (calc, ST)
-- foldr min >>> snd
