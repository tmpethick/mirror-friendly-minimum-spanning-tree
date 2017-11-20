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
       
main :: IO Solution
main = return $ simplex prob1 test1 []

-- compute 

test filename = do
  parsed <- parseGraphFile fileName
  let (gr, ws) = toGraph parsed
  putStrLn $ "Problem: " ++ fileName
  print w
  print mgr
