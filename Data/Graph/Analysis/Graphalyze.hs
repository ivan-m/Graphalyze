module Data.Graph.Analysis.Graphalyze where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.Maybe
import Control.Arrow((***))
import qualified Data.Map as M

type AGr a = Gr a ()

type Surround a = Context a ()

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

findNodesFor   :: (GraphData a -> [Surround a]) -> GraphData a -> [LNode a]
findNodesFor f = nodeOf . f

nodeOf :: (Functor f) => f (Surround a) -> f (LNode a)
nodeOf = fmap labNode'

single     :: [a] -> Bool
single [_] = True
single  _  = False

label :: LNode a -> a
label = snd

node :: LNode a -> Node
node = fst
