{-# LANGUAGE FlexibleContexts #-}
module MirrorFriendlyMinimumSpanningTree where

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

  paths' :: (Eq a) => a -> a -> [Edge a] -> [[a]]
  paths' a b xs | a == b = [[a]]
                | otherwise = concat [map (a :) $ paths' d b [x | x <- xs, x /= (c, d, e)]
                                      | (c, d, e) <- xs, c == a] ++ 
                              concat [map (a :) $ paths' c b [x | x <- xs, x /= (c, d, e)]
                                      | (c, d, e) <- xs, d == a]

  cycle' :: (Eq a) => a -> [Edge a] -> [[a]]
  cycle' a xs = [a : path | e <- xs, sel1 e == a, path <- paths' (sel2 e) a [x | x <- xs, x /= e]] ++
                [a : path | e <- xs, sel2 e == a, path <- paths' (sel1 e) a [x | x <- xs, x /= e]]

  powerset :: [a] -> [[a]]
  powerset [] = [[]]
  powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

  -- Assumed undirected
  -- Modifed version of answer to Problem 83: https://wiki.haskell.org/99_questions/80_to_89
  spantree :: (Eq a, Ord a) => Graph a -> [Graph a]
  spantree (Graph xs ys) = filter connected $ filter (not . cycles) $ filter nodes alltrees
    where
      alltrees = [Graph (extractNodes edges) edges | edges <- powerset ys]
      nodes (Graph xs' ys') = length xs == length xs'
      cycles (Graph xs' ys') = any ((/=) 0 . length . flip cycle' ys') xs'
      connected (Graph (x':xs') ys') = not $ any null [paths' x' y' ys' | y' <- xs']

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

  ------------------------
  -- Sum
  ------------------------
 
  weight       ws (_, _, i) = ws ! i
  mirrorWeight ws (_, _, i) = ws ! ((snd . bounds) ws - i)

  totalWeight :: Weights -> SpanningTree a -> Weight
  totalWeight ws st = max (sum $ fmap (weight ws) (edges st)) 
                          (sum $ fmap (mirrorWeight ws) (edges st))

  -- Uses `foldl1` instead `foldr1` to avoid storing all generated tree.
  -- See Notes in: https://hackage.haskell.org/package/base-4.10.0.0/docs/src/Data.Foldable.html#foldl1
  minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> a
  minimumBy' cmp = foldl1 min'
    where min' x y = case cmp x y of
                          GT -> y
                          _  -> x
                          
  mst :: Weights -> [SpanningTree a] -> (Weight, SpanningTree a)
  mst ws gs = minimumBy' (\x y -> fst x `compare` fst y) vg
              where vg = zip (fmap (totalWeight ws) gs) gs

  ------------------------
  -- Main
  ------------------------
  
  test fileName = do
    parsed <- parseGraphFile fileName
    let (gr, ws) = toGraph parsed
    let (w, mgr) = mst ws $ spantree gr
    putStrLn $ "Problem: " ++ fileName
    print w
    print mgr
    return (mgr, ws)
