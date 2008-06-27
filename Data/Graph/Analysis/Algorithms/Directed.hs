module Data.Graph.Analysis.Algorithms.Directed where

import Data.Graph.Analysis.Graphalyze    
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.List
import Data.Maybe
import Control.Arrow((***), first)
import Control.Monad(join,liftM2)
import qualified Data.Map as M

rootsOf :: AGr a -> [LNode a]
rootsOf = filterNodes isRoot
    where
      isRoot g = null . pre g . fst

findRoots :: GraphData a -> [LNode a]
findRoots = applyAlg rootsOf

classifyRoots    :: (Eq a) => GraphData a -> (Maybe (LNode a), [LNode a])
classifyRoots gr = first listToMaybe $ partition isWanted roots
    where
      roots = findRoots gr
      theRoot = wantedRoot gr
      isWanted = maybe (const False) (==) theRoot

wantedRootExists :: (Eq a) => GraphData a -> Bool
wantedRootExists = isJust . fst . classifyRoots

--------------

leavesOf :: AGr a -> [LNode a]
leavesOf = filterNodes isLeaf
    where
      isLeaf g = null . suc g . fst
                   
findLeaves :: GraphData a -> [LNode a]
findLeaves = applyAlg leavesOf

----

singletonsOf :: AGr a -> [LNode a]
singletonsOf = filterNodes isSingleton
    where
      isSingleton g = liftM2 (&&) (null . pre g) (null . suc g) . fst

findSingletons :: GraphData a -> [LNode a]
findSingletons = applyAlg singletonsOf

----

coreOf    :: (Eq a) => AGr a -> AGr a
coreOf gr = fixPoint strip gr
    where
      strip gr' = removeNodes roots $ removeNodes leaves $ gr'
          where
            roots = map node $ rootsOf gr'
            leaves = map node $ leavesOf gr'
            removeNode g = snd . flip match g
            removeNodes ns g = foldl' removeNode g ns

findCore :: (Eq a) => GraphData a -> AGr a
findCore = coreOf . graph

----

-- find connected components... technically undirected version

componentsOf    :: AGr a -> [AGr a]
componentsOf gr = map fst .
                  takeWhile' (not . isEmpty . snd) .
                  iterate splitComp $ initial
    where
      initial = (empty, gr)
      splitComp = splitComponent . snd

splitComponent :: AGr a -> (AGr a, AGr a)
splitComponent = first buildGr . extractNode . first Just . matchAny


extractNode :: ADecomp a -> ([AContext a], AGr a)
extractNode (Nothing,gr) = ([],gr)
extractNode (Just ctxt, gr)
    | isEmpty gr = ([ctxt], empty)
    | otherwise  = first (ctxt:) $ foldl' nodeExtractor ([],gr) nbrs
    where
      nbrs = neighbors' ctxt

nodeExtractor :: ([AContext a], AGr a) -> Node -> ([AContext a], AGr a)
nodeExtractor cg@(cs,g) n
    | gelem n g = first (cs ++) . extractNode $ match n g
    | otherwise = cg

findComponents :: GraphData a -> [AGr a]
findComponents = applyAlg componentsOf


-- to test with

a,b,c,d,e,f,g :: AGr Char

a = ([],1,'a',[]::[((),Node)]) & empty
b = ([((),1)],2,'b',[]) & a
c = ([((),1),((),2)],3,'c',[]) & b
d = ([],4,'d',[]::[((),Node)]) & c
e = ([((),1),((),2)],5,'e',[((),3)]) & d
f = ([((),1),((),2)],0,'f',[((),3)]) & e
g = ([((),2)],6,'g',[((),2)]) & f
