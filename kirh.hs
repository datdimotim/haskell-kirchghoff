{-# LANGUAGE FlexibleInstances, TupleSections, KindSignatures, DataKinds, ConstraintKinds #-}

module Kirh where

import Data.List (nub, sort, find, findIndex, (\\))
import Data.Maybe (isNothing)
import Control.Monad (forM_)
import Data.Maybe (fromJust)

type NodeCount = Int
type ResCount = Int

type NodeInd = Int

data Ln = Ln NodeInd NodeInd Int 


type Edge a = (NodeInd, NodeInd, a)

data EdgeVal = EdgeVal { getR :: Int, getU :: Int} deriving (Eq, Show)

instance EdgeValOrient EdgeVal where
  reverseEdgeVal (EdgeVal r u) = EdgeVal r (-u) 

edgeFrom :: Edge a -> NodeInd
edgeFrom (s,f,v) = s

edgeTo :: Edge a -> NodeInd
edgeTo (s,f,v) = f

edgeVal :: Edge a -> a
edgeVal (s,f,v) = v


edgeSign :: Edge a -> Int
edgeSign (s, f, _) | s < f = 1
                   | otherwise = (-1)

--readGraph :: NodeCount -> ResCount -> [Ln] -> Graph
--readGraph = undefined

kirh ::  NodeCount -> ResCount -> [Ln] -> Double
kirh = undefined

type EqSys a = [([(Edge a, Int)],Int)]


printSystem :: EqSys EdgeVal -> IO ()
printSystem ss = forM_ ss $ \ (vs, b) -> do
                                          let v = map (\((s,f,_),k) -> ((s,f), k)) vs
                                          print (v, b)

printMatrixView :: ([String], [([Int], Int)]) -> IO ()
printMatrixView (lbls, lns) = let
                              in
                                do
                                  forM_ (zip [1..] lbls) $ \(i, v) -> do
                                    putStrLn $ show i ++ ": " ++ v
                                  putStrLn ""
                                  forM_ lns $ \ln -> print ln

matrixFromEquations :: EqSys EdgeVal -> ([String], [([Int], Int)])
matrixFromEquations eqs = (varNames, lns) where
  printVar :: (NodeInd, NodeInd, EdgeVal) -> String
  printVar (s,f,_) = "I" ++ show (s,f)
  varNames = nub . map printVar . map fst . concatMap fst $ eqs
  size = length varNames
  varInd = fromJust . (\v -> findIndex (==v) varNames) . printVar
  ln :: ([(Edge EdgeVal, Int)], Int) -> ([Int], Int)
  ln (es, r) = let
                 add (e, c) f = \i -> if varInd e == i - 1
                                      then c
                                      else f i 
                 indVarMap = foldr add (const 0) es 
               in
                 (map indVarMap [1..size], r)
  lns = map ln eqs


buildSystem :: Graph g => g EdgeVal-> EqSys EdgeVal
buildSystem g = buildContourEquations g ++ buildNodeEquations g




buildContourEquations :: Graph g => g EdgeVal -> EqSys EdgeVal
buildContourEquations g = let
                            ccs = findContours g
                            edgePart e@(s, f, EdgeVal r u) = (e, r)
                            contourEquation = map edgePart
                            contourU = (0-) . sum . map (\e@(s, f, EdgeVal r u) -> u)
                          in
                            map (\cs -> (map normalizeDir . contourEquation $ cs, contourU cs)) ccs
                           

buildNodeEquations :: Graph g => g EdgeVal -> [([(Edge EdgeVal, Int)], Int)]
buildNodeEquations g = let
                         ns = tail (nodes g)
                         nodeEquation n = map (\e -> (e, edgeSign e)) (edgesFrom g n)
                         nodeEquationNormalizedVars = map normalizeDir . nodeEquation
                       in
                         map ((,0) . nodeEquationNormalizedVars) $ ns
                         
normalizeDir :: (EdgeValOrient a) => (Edge a, Int) -> (Edge a, Int)
normalizeDir (e,v) = let
                       isStdOrient (s, f, _) = s < f
                     in
                       if isStdOrient e
                       then (e, v)
                       else (reverseEdge e, (-v))  


findTree :: (Graph g, EdgeValOrient a) => g a -> [Edge a]
findTree g = helper [] where
  eds = edges g
  helper t = let
               vs = nodes $ mkEdgeListGraph t
               e' = [e | e@(s,f,_) <- eds , not (elem s vs) || not (elem f vs)]
             in 
               if null e'
               then t
               else helper (head e' : t)
  

findContours :: (Graph g, EdgeValOrient a) => g a -> [[Edge a]]
findContours g = let
                   t = mkEdgeListGraph $ findTree g
                   rs = filter (\(s,f,_) -> f > s) $ edges g \\ edges t
                   contourByEdge e = findContoursWithEdge (e `plusEdge` t) e
                 in
                   map contourByEdge rs
                   

                              
containsUnorient :: Edge a -> [Edge a] -> Bool
containsUnorient (s, f, _) es = let
                                  ps = map (\(a,b,_) -> (a,b)) es
                                in
                                  ((s,f) `elem` ps) || ((f,s) `elem` ps) 

findContoursWithEdge :: Graph g => g a -> Edge a -> [Edge a]
findContoursWithEdge g e@(s, f, _) = head $ findContoursWithEdgePath g f s [e]


findContoursWithEdgePath :: Graph g => g a -> NodeInd -> NodeInd -> [Edge a] -> [[Edge a]]
findContoursWithEdgePath g c f p | c == f = [reverse p]
                                 | otherwise = let
                                                 ns = filter (not . flip containsUnorient p)
                                                   . (edgesFrom g) 
                                                   $ c 
                                               in
                                                 do
                                                   n@(_, c', _) <- ns
                                                   let p' = n : p
                                                   findContoursWithEdgePath g c' f p'
                              


                                                   
g2 :: EdgeListGraph EdgeVal
g2 = mkEdgeListGraph 
    [
     (1, 2, EdgeVal 10 0), -- R1
     (1, 3, EdgeVal 10 0), -- R3
     (1, 4, EdgeVal 0 40), -- E3
     (2, 3, EdgeVal 0 10), -- E1
     (2, 4, EdgeVal 10 0), -- R2
     (3, 4, EdgeVal 0 20)  -- E2
    ]                                            
{-                                 Answer:
    __R1__2___R2__     R1=10        I12 = 1
   |      +       |    R2=10        I24 = 3
   |      E1      |    R3=10        I13 = 2
   1__R3__|__+E2__|    E1=10       I23 = -2
   |      3       4    E2=20       I34 = 0
   |              |    E3=40       I14 =-3
   |_____+E3______|

   1____2____5
   |    |    |
   |____|____|
   4    3    6             
-}      
g1 :: EdgeListGraph EdgeVal
g1 = mkEdgeListGraph . map (\(s,f) -> (s,f,EdgeVal 13 0)) $ 
    [
     (1,2),
     (1,4),
     (2,3),
     (2,5),
     (3,4),
     (3,6),
     (5,6)
    ]
        
        
class Graph g where
  nodeCount :: g a -> Int
  nodeCount = length . nodes
  
  edgeCount :: g a -> Int
  edgeCount = length . edges
  
  edges :: g a -> [Edge a]
  
  fromEdges :: EdgeValOrient a => [Edge a] -> g a
  
  nodes :: g a -> [NodeInd]
  nodes = nub . sort . concatMap (\(s,f,_) -> [s,f]) . edges
  
  edgesFrom :: g a -> NodeInd -> [Edge a]
  edgesFrom g  n = filter ((== n) . edgeFrom) . edges $ g
  
  edgesTo :: g a -> NodeInd -> [Edge a]
  edgesTo g n = filter ((== n) . edgeTo) . edges $ g
  
  getEdgeVal :: g a -> (NodeInd, NodeInd) -> Maybe a
  getEdgeVal g (s, f) = fmap edgeVal . find (\(s', f', _) -> s == s' && f == f') . edges $ g
  
  plusEdge :: EdgeValOrient a => Edge a -> g a -> g a
  plusEdge e = fromEdges . ([e, reverseEdge e] ++ ) . edges
  

reverseEdge :: EdgeValOrient a => Edge a -> Edge a
reverseEdge (s,f,v) = (f,s,reverseEdgeVal v)  

class Eq v => EdgeValOrient v where
  reverseEdgeVal :: v -> v

newtype EdgeListGraph a = EdgeListGraph {getEdgeList :: [Edge a]}

mkEdgeListGraph :: EdgeValOrient a => [Edge a] -> EdgeListGraph a
mkEdgeListGraph = EdgeListGraph . concatMap (\e -> [e, reverseEdge e])

instance Graph EdgeListGraph where
  edges = getEdgeList
  fromEdges = mkEdgeListGraph
  
  
