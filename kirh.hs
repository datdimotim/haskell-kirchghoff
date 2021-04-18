{-# LANGUAGE FlexibleInstances, TupleSections #-}

module Kirh where

import Data.List (nub, sort, find, (\\))
import Data.Maybe (isNothing)
import Control.Monad (forM_)

type NodeCount = Int
type ResCount = Int

type NodeInd = Int

data Ln = Ln NodeInd NodeInd Int 


type Edge = (NodeInd, NodeInd, EdgeVal)

data EdgeVal = EdgeVal { getR :: Int, getU :: Int} deriving (Eq, Show)

reverseEdge :: Edge -> Edge
reverseEdge (s, f, EdgeVal r u) = (f, s, EdgeVal r (-u)) 

edgeFrom :: Edge -> NodeInd
edgeFrom (s,f,v) = s

edgeTo :: Edge -> NodeInd
edgeTo (s,f,v) = f

edgeVal :: Edge -> EdgeVal
edgeVal (s,f,v) = v


edgeSign :: Edge -> Int
edgeSign (s, f, _) | s < f = 1
                   | otherwise = (-1)

--readGraph :: NodeCount -> ResCount -> [Ln] -> Graph
--readGraph = undefined

kirh ::  NodeCount -> ResCount -> [Ln] -> Double
kirh = undefined

type EqSys = [([(Edge, Int)],Int)]

printSystem :: EqSys -> IO ()
printSystem ss = forM_ ss $ \ (vs, b) -> do
                                          let v = map (\((s,f,_),k) -> ((s,f), k)) vs
                                          print (v, b)



buildSystem :: Graph g => g -> EqSys
buildSystem g = buildContourEquations g ++ buildNodeEquations g


buildContourEquations :: Graph g => g -> EqSys
buildContourEquations g = let
                            ccs = findContours g
                            edgePart e@(s, f, EdgeVal r u) = (e, r)
                            contourEquation cs = map edgePart cs
                            contourU = sum . map (\e@(s, f, EdgeVal r u) -> u)
                          in
                            map (\cs -> (contourEquation cs, contourU cs)) ccs

buildNodeEquations :: Graph g => g -> [([(Edge, Int)], Int)]
buildNodeEquations g = let
                         ns = tail (nodes g)
                         nodeEquation n = map (\e -> (e, edgeSign e)) (edgesFrom g n)
                       in
                         map ((,0) . nodeEquation) ns


findTree :: Graph g => g -> [Edge]
findTree g = helper [] where
  eds = edges g
  helper t = let
               vs = nodes t
               e' = [e | e@(s,f,_) <- eds , not (elem s vs) || not (elem f vs)]
             in 
               if null e'
               then t
               else helper (head e' : t)
  

findContours :: Graph g => g -> [[Edge]]
findContours g = let
                   t = findTree g
                   rs = filter (\(s,f,_) -> f > s) $ edges g \\ edges t
                   contourByEdge e = findContoursWithEdge (e:t) e
                 in
                   map contourByEdge rs
                   

                              
containsUnorient :: Edge -> [Edge] -> Bool
containsUnorient (s, f, _) es = let
                                  ps = map (\(a,b,_) -> (a,b)) es
                                in
                                  ((s,f) `elem` ps) || ((f,s) `elem` ps) 

findContoursWithEdge :: Graph g => g -> Edge -> [Edge]
findContoursWithEdge g e@(s, f, _) = head $ findContoursWithEdgePath g f s [e]


findContoursWithEdgePath :: Graph g => g -> NodeInd -> NodeInd -> [Edge] -> [[Edge]]
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
                              


                                                   
                                                   
{-
   1____2____5
   |    |    |
   |____|____|
   4    3    6             
-}      
g1 :: [Edge]
g1 = map (\(s,f) -> (s,f,EdgeVal 0 0)) 
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
  nodeCount :: g -> Int
  nodeCount = length . nodes
  
  edgeCount :: g -> Int
  edgeCount = length . edges
  
  edges :: g -> [Edge]
  
  nodes :: g -> [NodeInd]
  nodes = nub . sort . concatMap (\(s,f,_) -> [s,f]) . edges
  
  edgesFrom :: g -> NodeInd -> [Edge]
  edgesFrom g n = filter ((== n) . edgeFrom) . edges $ g
  
  edgesTo :: g -> NodeInd -> [Edge]
  edgesTo g n = filter ((== n) . edgeTo) . edges $ g
  
  getEdgeVal :: g -> (NodeInd, NodeInd) -> Maybe EdgeVal
  getEdgeVal g (s, f) = fmap edgeVal . find (\(s', f', _) -> s == s' && f == f') . edges $ g
  
  
  
  
  
instance Graph [Edge] where
  edges = nub . concatMap (\e -> [e, reverseEdge e])
  
  
