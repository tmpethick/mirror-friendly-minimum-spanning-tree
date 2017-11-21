module LinearMFMST where

import Numeric.LinearProgramming
import Data.Array
import TreeParser
import Data.Tuple.Select


toTree gr es = Graph (nodes gr) newEdges
  where 
    newEdges = fmap snd $ filter (\x -> fst x == 1) $ zip es (edges gr)

-- compute 
minimize fileName = do
  parsed <- parseGraphFile fileName
  let (gr, ws) = toGraph parsed
  putStrLn $ "Problem: " ++ fileName
  print gr
  let mn = minimization gr
  let cs = [ numberOfEdeges gr
           , constraintB ws
           , constraintBMirror ws] ++ (nodeEquations gr)
  let bs = constraintBounds gr
  let result = simplex (Minimize mn) (Dense cs) bs
  case result of 
    Optimal (b, s) -> do 
      print b
      let edges = fmap round $ tail s 
      print edges
      let tree = toTree gr edges
      print $ sum edges
      let weight = totalWeight ws tree
      print weight
    _              -> print "Unable to find solution"

-- Creates the optimization for the graph
minimization :: Graph a -> [Double]
minimization gr = 1 : replicate ((length . edges) gr) 0

-- Creates the equations for each node in the graph
nodeEquations :: Graph Integer -> [Bound [Double]]
nodeEquations gr = fmap (\x -> nodeConstraint ((reverse . edges) gr) x) (es gr)
  where 
    edgeMember :: Edge Integer -> Integer -> Bool 
    edgeMember d x = sel1 d == x || sel2 d == x
    nodeConstraint :: [Edge Integer] -> Integer -> Bound [Double]
    nodeConstraint es n = (0 : fmap (\x -> if edgeMember x n then 1 else 0) es) :>=: 1
    es :: Graph Integer -> [Integer]
    es gr = [1..((toInteger . length . nodes) gr)]

-- Creates the equation to control the number of edges in the tree
limit = 100000000000
numberOfEdeges :: Graph a -> Bound [Double]
numberOfEdeges gr = (0 : replicate count (fromIntegral limit)) :==: fromIntegral ((((length . nodes) gr) - 1) * limit)
    where count = ((length . edges) gr)

-- Creates the equations to constraint B from the weights of the edges
constraintB :: Weights -> Bound [Double]
constraintB ws = (1 : fmap (negate . fromIntegral) (elems ws)) :>=: 0

constraintBMirror :: Weights -> Bound [Double]
constraintBMirror ws = (1 : fmap (negate . fromIntegral) ((reverse . elems) ws)) :>=: 0

-- Create bounds for all the edges to be between 0 and 1
constraintBounds :: Graph a -> Bounds
constraintBounds gr = fmap (\x -> x :&: (0,1)) [2..((length . edges) gr) + 1]
