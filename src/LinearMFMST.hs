module LinearMFMST where

import Numeric.LinearProgramming

import TreeParser

prob1 = Minimize [1, 0, 0, 0]
test1 = Dense [ [0, 1, 0, 1] :>=: 1
              , [0, 1, 1, 0] :>=: 1
              , [0, 0, 1, 1] :>=: 1 
              -- Equations n - 1
              , [0, 1000000, 1000000, 1000000] :==: 2000000
              -- Equations to calculate B
              , [1,-1,-2,-3] :>=: 0
              , [1,-3,-2,-1] :>=: 0
              ]
        
-- main :: IO Solution
-- main = return $ simplex prob1 test1 []

-- compute 

test fileName = do
  parsed <- parseGraphFile fileName
  let (gr, ws) = toGraph parsed
  putStrLn $ "Problem: " ++ fileName
  print gr
  return 3

-- Creates the optimization for the graph
minimization :: Graph a -> Optimization
minimization gr = Minimize $ 1 : replicate ((length . edges) gr) 0

-- Creates the equations for each node in the graph
nodeEquations :: Graph a -> [Double]
nodeEquations gr = undefined

-- Creates the equation to control the number of edges in the tree
numberOfEdeges :: Graph a -> [Double]
numberOfEdeges gr = undefined

-- Creates the equations to constraint B from the weights of the edges
constraintB :: Graph a -> [Double]
constraintB gr = undefined
