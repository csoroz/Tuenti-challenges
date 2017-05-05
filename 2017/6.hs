{-# LANGUAGE TupleSections #-}
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree 
import Data.Graph.Inductive.Query.SP
import Data.List
import Data.Ord

type I = Integer

type Case = (Node,[LEdge I])

nubSort :: Ord a => [a] -> [a]
nubSort = map head . group . sort

solve :: Case -> I
solve (n,xs) = spLength 1 n gr
  where
    gr = mkGraph (map (,()) nodes) labEdges :: Gr () I
    labEdges = xs ++ ys ++ zs
    es = map toEdge xs
    as = map fst es
    bs = map snd es
    nodes = nubSort $ 1:n:(as ++ bs)
    ys = map f (zip nodes $ tail nodes)
      where f (a,b) = (a,b, l (fromIntegral a) (fromIntegral b))
            l a b = ((b - a)*(a + b - 1)) `div` 2
    zs = map zero $ nubSort $ concatMap g es
      where
        zero (a,b) = (a,b,0)
        g (a,b) = map ((b,).fst) $ filter p es
          where p (x,y) = x > a && x < b && y > b

byLines f = interact $ unlines . f . lines

showCase (i,x) = concat ["Case #",show i,": ", show x]

parse [] = []
parse (x:xs) = (read n, map g ys) : parse zs
  where
    [n,k] = take 2 $ words x
    (ys,zs) = splitAt (read k) xs
    g = f . take 3 . words
      where f [a,b,y] = (read a, read b, read y)

main = byLines $ map showCase . zip [1..] . map solve . g
  where g (l:ls) = take (read l) (parse ls)
