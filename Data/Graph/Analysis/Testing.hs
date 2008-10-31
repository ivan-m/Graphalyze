{- |
   Module      : Data.Graph.Analysis.Testing
   Description : Testing module
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is for testing purposes only,
   i.e. you probably shouldn't be accessing it.
 -}
module Data.Graph.Analysis.Testing
    ( module Data.Graph.Analysis.Visualisation,
      a,b,c,d,e,f,g,h,i,j,k,
      z,y,x,w,v,u,t,s,r,q,p,o,n,
      randomGraph,
      preview,
      printList
    ) where

import Data.Graph.Analysis
import Data.Graph.Analysis.Visualisation
import Data.GraphViz
import System.Process
import System.Random


printList :: (Show a) => [a] -> IO ()
printList = mapM_ (putStrLn . show)


a,b,c,d,e,f,g,h,i,j,k :: AGr Char
a = ([],1,'a',[]::[((),Node)]) & empty
b = ([((),1)],2,'b',[]) & a
c = ([((),1),((),2),((),1)],3,'c',[]) & b
d = ([],4,'d',[]::[((),Node)]) & c
e = ([((),1),((),2)],5,'e',[((),3)]) & d
f = ([((),1),((),2)],0,'f',[((),3)]) & e
g = ([((),2)],6,'g',[((),2)]) & f
h = ([((),7)],7,'h',[]) & g
i = ([((),0)],8,'i',[((),6)]) & h
j = ([((),0)],9,'j',[((),6)]) & i
k = ([((),2),((),6),((),9)],10,'k',[((),2),((),6),((),9)]) & j

z,y,x,w,v,u,t,s,r,q,p,o :: AGr Char
z = ([],1,'z',[]) & empty
y = ([((),1)],2,'y',[]) & z
x = ([((),1)],3,'x',[]) & y
w = ([((),2)],4,'w',[]) & x
v = ([((),2)],5,'v',[]) & w
u = ([((),3)],6,'u',[]) & v
t = ([((),3)],7,'t',[]) & u
s = ([],8,'s',[((),1)]) & t
r = ([((),2),((),3)],9,'r',[]) & s
q = ([((),8)],10,'q',[]) & r
p = ([((),9)],11,'p',[((),9)]) & q
o = ([((),2)],12,'o',[]) & p
n = ([((),11)],13,'n',[((),11)]) & o


preview   :: DotGraph -> IO ()
preview d = do runGraphviz d Png imgFile
               runCommand ("display " ++ imgFile)
               return ()
    where
      imgFile = loc++file++".png"
      file = "dotGraph"
      loc = "/tmp/"

randomGraph     :: (RandomGen g) => Int -> g -> AGr Int
randomGraph n g = mkGraph ns' es''
    where
      ns = [1..n]
      ns' = map (\n -> (n,n)) ns
      rs :: [Double]
      rs = randoms g
      p = 10 / (fromIntegral n)
      es = [ (n1,n2) | n1 <- ns, n2 <- ns, n1 /= n2 ]
      es' = map fst . filter ((<= p) . snd) $ zip es rs
      es'' = map (\(n1,n2) -> (n1,n2,())) es'
