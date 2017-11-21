module Main where

import Control.Exception
import Data.Time
import System.Environment
import qualified LinearMFMST as L


timer m = do 
  start <- getCurrentTime
  m
  end <- getCurrentTime
  print (diffUTCTime end start)

timedTest fileName = timer (L.minimize fileName)


main :: IO ()
main = do
  graphFileNames <- getArgs
  mapM_ timedTest graphFileNames

