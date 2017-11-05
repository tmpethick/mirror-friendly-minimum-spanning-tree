{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module MirrorFriendlyMinimumSpanningTree where

  import Data.Array
  import qualified Data.Set as Set
  import Control.Monad.IO.Class
  import Debug.Trace
  import Data.List
  import Control.Arrow ((>>>))
  import Data.List.Unique
  import Data.Tuple.Select
  import Control.Monad (void)
  import Text.Parsec
  import Text.Parsec.String
  import System.Exit

  ------------------------
  -- Graph
  ------------------------
 
  -- Modified from                https://mfukar.github.io/2015/09/15/haskellxii.html
  -- As answer to Problem 83 from https://wiki.haskell.org/99_questions/80_to_89
  -- Assumed undirected
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

  spantree :: (Eq a, Ord a) => Graph a -> [Graph a]
  spantree (Graph xs ys) = filter connected $ filter (not . cycles) $ filter nodes alltrees
    where
      alltrees = [Graph (extractnodes edges) edges | edges <- powerset ys]
      nodes (Graph xs' ys') = length xs == length xs'
      cycles (Graph xs' ys') = any ((/=) 0 . length . flip cycle' ys') xs'
      connected (Graph (x':xs') ys') = not $ any null [paths' x' y' ys' | y' <- xs']

  -- Test case from divide and conquer
  nodes' = [0..5]
  edges' = [(1,2,1+1),
            (1,3,2+1),
            (1,4,3+1),
            (1,6,4+1),
            (2,3,5+1),
            (2,5,6+1),
            (3,4,7+1),
            (4,5,8+1)]
    
  ------------------------
  -- Parser
  ------------------------

  -- To allow space at the end of a line
  strictSpaces :: (Stream s m Char) => ParsecT s u m ()  
  strictSpaces = skipMany $ oneOf " "

  lineEnd :: (Stream s m Char) => ParsecT s u m ()
  lineEnd = void $ strictSpaces >> endOfLine

  -- file :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
  -- file p = (p <* lineEnd) <* eof
  
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
   
  -- testParse = parse parser "()" "1\n2\n1 2 3\n1 2 3"

  mkUniq :: Ord a => [a] -> [a]
  mkUniq = Set.toList . Set.fromList
  
  extractnodes :: (Ord a) => [Edge a] -> [a]
  extractnodes edges = mkUniq $ fmap sel1 edges ++ fmap sel2 edges

  toGraph :: Ord a => (Integer, Integer, [Edge a]) -> (Graph a, Weights)
  toGraph (n, m, edges) = (Graph (extractnodes edges) (snd edgesWithIx), weights)
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
  
  main = do
    parsed <- parseGraphFile "./data/test/TestFile1.uwg"
    -- parsed <- parseGraphFile "../data/test/TestFile8.uwg"
    let (gr, ws) = toGraph parsed
    let (w, mgr) = mst ws $ spantree gr
    print w
    print $ length $ edges mgr
    print $ length $ nodes mgr
    return (mgr, ws)

    -- parsed <- parseGraphFile "../data/test/TestFile1.uwg"
    -- (gr, ws) = toGraph parsed
    -- mst ws gr
 