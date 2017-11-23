module LinearMFMST where

import Numeric.LinearProgramming
import Data.Array
import Data.Tuple.Select
import Control.Monad

import TreeParser


toTree gr es = Graph (nodes gr) newEdges
  where
    newEdges = fmap snd $ filter (\x -> fst x == 1) $ zip es (edges gr)

-- compute
minimize fileName = do
  parsed <- parseGraphFile fileName
  let (gr, ws) = toGraph parsed
  putStrLn $ "Problem: " ++ fileName
  -- print gr
  let mn = minimization gr
  let cs = [ constraintEdges gr
           , constraintB ws
           , constraintBMirror ws] ++ (constraintNodes gr)
  let result = simplex (Minimize mn) (Dense cs) (constraintBounds gr)
  -- mapM_ print $ constraintNodes gr
  case result of
    Optimal (b, s) -> do
      print s
      let edges = fmap (round) $ tail s
      print edges
      print $ elems ws
      print "Verifying equations"
      let b = verifyEquations gr (0 : edges)
      if b
        then print $ totalWeight' ws edges
        else print "Not a valid solution"
    _              -> print "Unable to find solution"

listProduct :: Num a => [a] -> [a] -> [a]
listProduct = zipWith (*)

verifyEquations :: Graph Integer -> [Int] -> Bool
verifyEquations gr vars = and (correctNumberOfEdges : nodesEqs)
  where
    nodesEqs = fmap (\y -> sum y >= 1) $ fmap (listProduct vars) (nodeEquations gr)
    correctNumberOfEdges = sum vars == (((length . nodes) gr) - 1)

totalWeight' :: Weights -> [Int] -> Int
totalWeight' ws es = max normal mirror
  where
    normal = sum $ listProduct es           $ fmap fromIntegral $ elems ws
    mirror = sum $ listProduct (reverse es) $ fmap fromIntegral $ elems ws


-- Creates the optimization for the graph
minimization :: Graph a -> [Double]
minimization gr = 1 : replicate ((length . edges) gr) 0

-- Creates the equations for each node in the graph
nodeEquations :: (Num a) => Graph Integer -> [[a]]
nodeEquations gr = fmap (\x -> nodeConstraint ((reverse . edges) gr) x) (es gr)
  where
    edgeMember :: Edge Integer -> Integer -> Bool
    edgeMember d x = sel1 d == x || sel2 d == x
    -- nodeConstraint :: [Edge Integer] -> Integer -> [a]
    nodeConstraint es n = (0 : fmap (\x -> if edgeMember x n then 1 else 0) es)
    es :: Graph Integer -> [Integer]
    es gr = [1..((toInteger . length . nodes) gr)]

constraintNodes :: Graph Integer -> [Bound [Double]]
constraintNodes gr = fmap (:>=: 1) $ nodeEquations gr

-- Creates the equation to control the number of edges in the tree
limit = 100000000000
numberOfEdeges :: Num b => Graph a -> [b]
numberOfEdeges gr = (0 : replicate count (fromIntegral limit))
    where count = ((length . edges) gr)

constraintEdges :: Graph a -> Bound [Double]
constraintEdges gr = (numberOfEdeges gr) :==: edgesCount
  where edgesCount = fromIntegral ((((length . nodes) gr) - 1) * limit)

-- Creates the equations to constraint B from the weights of the edges
equationB :: Num a => Weights -> [a]
equationB ws = (1 : fmap (negate . fromIntegral) (elems ws))

equationBMirror :: Num a => Weights -> [a]
equationBMirror ws = (1 : fmap (negate . fromIntegral) ((reverse . elems) ws))

constraintB :: Weights -> Bound [Double]
constraintB ws = equationB ws :>=: 0

constraintBMirror :: Weights -> Bound [Double]
constraintBMirror ws = equationBMirror ws :>=: 0

-- Create bounds for all the edges to be between 0 and 1
constraintBounds :: Graph a -> Bounds
constraintBounds gr = fmap (\x -> x :&: (0,1)) [2..((length . edges) gr) + 1]
