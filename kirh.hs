{-# LANGUAGE FlexibleInstances #-}

module Kirh where

import Data.List (nub, sort, find, (\\))
import Data.Maybe (isNothing)

type NodeCount = Int
type ResCount = Int

type NodeInd = Int

data Ln = Ln NodeInd NodeInd Int 


type Edge = (NodeInd, NodeInd, EdgeVal)

data EdgeVal = EdgeVal { getR :: Int, getU :: Int} deriving (Eq, Show)

reverseEdge :: Edge -> Edge
reverseEdge (s,f,v) = (f,s,v) 

edgeFrom :: Edge -> NodeInd
edgeFrom (s,f,v) = s

edgeTo :: Edge -> NodeInd
edgeTo (s,f,v) = f

edgeVal :: Edge -> EdgeVal
edgeVal (s,f,v) = v

--readGraph :: NodeCount -> ResCount -> [Ln] -> Graph
--readGraph = undefined

kirh ::  NodeCount -> ResCount -> [Ln] -> Double
kirh = undefined



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
                              
--containsUnorient :: Edge -> [Edge] -> Bool
--containsUnorient e es = (e `elem` es) || (reverseEdge e `elem` es) 

                                                   
                                                   
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
  
  
