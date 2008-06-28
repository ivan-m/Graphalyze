module Data.Graph.Analysis.Graphalyze where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.Maybe
import Control.Arrow((***))
import Control.Monad(join,liftM2,ap)
import qualified Data.Map as M

type AGr a = Gr a ()

-- Used by path-dependent algs
type PGr a = Gr a Int

type AContext a = Context a ()
type ADecomp a = Decomp Gr a ()

data GraphData a = GraphData { graph :: AGr a,
                               wantedRoot :: Maybe (LNode a)
                             }
                   deriving (Show)

data ImportParams a = Params { dataPoints :: [a],
                               relationships :: [(a,a)],
                               root :: Maybe a,
                               directed :: Bool
                             }

defaultParams :: ImportParams a
defaultParams = Params { dataPoints = [],
                         relationships = [],
                         root = Nothing,
                         directed = True
                       }

importData        :: (Ord a) => ImportParams a -> GraphData a
importData params = GraphData { graph = dGraph, wantedRoot = rootNode }
    where 
      lNodes = zip [1..] (dataPoints params) 
      nodeMap = foldr (uncurry (flip M.insert)) M.empty lNodes
      findNode n = M.lookup n nodeMap
      validEdge edg = case edg of
                        (Just x, Just y) -> Just (x,y)
                        _                -> Nothing
      addLabel (x,y) = (x,y,())
      graphEdges = catMaybes $ map (fmap addLabel . validEdge . (findNode *** findNode)) (relationships params)
      setDirection = if (directed params) then id else undir
      dGraph = setDirection $ mkGraph lNodes graphEdges
      rootNode = (root params) >>= (\ lab -> do {n <- findNode lab; return (n,lab) } ) --(\ lab -> fmap ((,) lab) (findNode lab))
      
nodeLabel      :: GraphData a -> Node -> Maybe a
nodeLabel gr n = lab (graph gr) n

findNodesFor   :: (GraphData a -> [AContext a]) -> GraphData a -> [LNode a]
findNodesFor f = nodeOf . f

nodeOf :: (Functor f) => f (AContext a) -> f (LNode a)
nodeOf = fmap labNode'

single     :: [a] -> Bool
single [_] = True
single  _  = False

label :: LNode a -> a
label = snd

node :: LNode a -> Node
node = fst

fixPoint     :: (Eq a) => (AGr a -> AGr a) -> AGr a -> AGr a
fixPoint f x = if (equal x x')
               then x'
               else fixPoint f x'
    where
      x' = f x

-- same as takeWhile, but include the first invalid element
           
takeWhile'      :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = [x]

filterNodes     :: (AGr a -> LNode a -> Bool) -> AGr a -> [LNode a]
filterNodes p g = filter (p g) (labNodes g)


applyAlg   :: (AGr a -> b) -> GraphData a -> b
applyAlg f = f . graph

toPathTree :: AGr a -> PGr a
toPathTree = emap (const 1)

pathValues          :: LPath a -> [LNode a]
pathValues (LP lns) = lns

addLabels    :: (Graph gr) => gr a b -> [Node] -> [LNode a]
addLabels gr = map (ap (,) (fromJust . lab gr))

-- to test with

a,b,c,d,e,f,g,h,i :: AGr Char

a = ([],1,'a',[]::[((),Node)]) & empty
b = ([((),1)],2,'b',[]) & a
c = ([((),1),((),2)],3,'c',[]) & b
d = ([],4,'d',[]::[((),Node)]) & c
e = ([((),1),((),2)],5,'e',[((),3)]) & d
f = ([((),1),((),2)],0,'f',[((),3)]) & e
g = ([((),2)],6,'g',[((),2)]) & f
h = ([((),7)],7,'h',[]) & g
i = ([((),0)],8,'i',[((),6)]) & h
