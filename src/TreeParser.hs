{-# LANGUAGE FlexibleContexts #-}
module TreeParser where

import Data.Array
import Data.List
import Data.Tuple.Select
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.String

import Control.Arrow ((>>>))
import Control.Monad (void)
import Control.Monad.IO.Class 

import System.Exit
import Debug.Trace

------------------------
-- Graph
------------------------

type EdgeIx = Integer
type Weight = Integer
type Weights = Array EdgeIx Weight

type Edge a = (a, a, EdgeIx)
data Graph a = Graph [a] [Edge a]
                deriving (Show, Eq)

type SpanningTree a = Graph a

nodes :: Graph a -> [a]
nodes (Graph ns _) = ns

edges :: Graph a -> [Edge a]
edges (Graph _ es) = es

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

extractNodes :: (Ord a) => [Edge a] -> [a]
extractNodes edges = unique $ fmap sel1 edges ++ fmap sel2 edges

weight       ws (_, _, i) = ws ! i
mirrorWeight ws (_, _, i) = ws ! ((snd . bounds) ws - i)

totalWeight :: Weights -> SpanningTree a -> Weight
totalWeight ws st = max (sum $ fmap (weight ws) (edges st)) 
                        (sum $ fmap (mirrorWeight ws) (edges st))

------------------------
-- Parser
------------------------

-- To allow space at the end of a line
strictSpaces :: (Stream s m Char) => ParsecT s u m ()  
strictSpaces = skipMany $ oneOf " "

lineEnd :: (Stream s m Char) => ParsecT s u m ()
lineEnd = void $ strictSpaces >> endOfLine

line :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
line p = p <* lineEnd

num :: Stream s m Char => ParsecT s u m Integer
num = read <$> many1 digit

tripletNum :: Stream s m Char => ParsecT s u m (Edge Integer)
tripletNum = (,,) <$> (num <* space) <*> (num <* space) <*> num

parser :: Stream s m Char => ParsecT s u m (Integer, Integer, [Edge Integer])
parser = do 
  n <- line num
  m <- line num
  edges <- tripletNum `sepEndBy` (lineEnd <|> eof) -- Format: `Node Node Weight`
  return (n, m, edges)

toGraph :: Ord a => (Integer, Integer, [Edge a]) -> (Graph a, Weights)
toGraph (n, m, edges) = (Graph (extractNodes edges) (snd edgesWithIx), weights)
  where 
    edgesWithIx =                        foldr (\e (i, s) -> (i + 1, (sel1 e, sel2 e, i) : s)) (0, []) (reverse edges)
    weights     = array (0, m-1) $ snd $ foldr (\e (i, s) -> (i + 1, (i, sel3 e)         : s)) (0, []) (reverse edges)

parseFile :: Parser a -> String -> IO a
parseFile p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
        traceIO $ "Error: " ++ show err
        exitFailure

parseGraphFile :: String -> IO (Integer, Integer, [Edge Integer])
parseGraphFile = parseFile parser