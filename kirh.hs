{-# LANGUAGE FlexibleInstances #-}

module Kirh where

import Data.List (nub, sort, find, (\\))
import Data.Maybe (isNothing)

type NodeCount = Int
type ResCount = Int

type NodeInd = Int

data Ln = Ln NodeInd NodeInd Int 


type Edge = (NodeInd, NodeInd, Int)

reverseEdge :: Edge -> Edge
reverseEdge (s,f,v) = (f,s,v) 

edgeFrom :: Edge -> NodeInd
edgeFrom (s,f,v) = s

edgeTo :: Edge -> NodeInd
edgeTo (s,f,v) = f

edgeVal :: Edge -> Int
edgeVal (s,f,v) = v

--readGraph :: NodeCount -> ResCount -> [Ln] -> Graph
--readGraph = undefined

kirh ::  NodeCount -> ResCount -> [Ln] -> Double
kirh = undefined

type Cycle = [Edge]


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
  


findContours :: Graph g => g -> [Cycle]
findContours g = findC (edges g) where
  findC [] = []
  findC (e:es) = let
                   c = findContoursWithEdge g e
                   es' = es \\ concatMap (\i -> [i, reverseEdge i]) c
                 in 
                   c : findC es'

                              
containsUnorient :: Edge -> [Edge] -> Bool
containsUnorient e es = (e `elem` es) || (reverseEdge e `elem` es) 

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
g1 = [
 (1,2,0),
 (1,4,0),
 (2,3,0),
 (2,5,0),
 (3,4,0),
 (3,6,0),
 (5,6,0)]
        
        
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
  
  getEdgeVal :: g -> (NodeInd, NodeInd) -> Maybe Int
  getEdgeVal g (s, f) = fmap edgeVal . find (\(s', f', _) -> s == s' && f == f') . edges $ g
  
  
  
  
  
instance Graph [Edge] where
  edges = nub . concatMap (\e -> [e, reverseEdge e])
  
  
