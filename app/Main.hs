module Main where
  import qualified MirrorFriendlyMinimumSpanningTree as M
  import Control.Exception
  import Data.Time
  import System.Environment

  timer m = do 
    start <- getCurrentTime
    m
    end <- getCurrentTime
    print (diffUTCTime end start)

  timedTest fileName = timer (M.test fileName)

  main :: IO ()
  main = do
    graphFileNames <- getArgs
    mapM_ timedTest graphFileNames
