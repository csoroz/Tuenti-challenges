import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph
import Data.List
import Data.Maybe

pairs xs = zip xs (tail xs)

groupPairs :: Eq a => [(a,b)] -> [(a,[b])]
groupPairs = map (g . unzip) . groupBy fstEQ
  where g (xs,ys) = (head xs, ys)
        fstEQ a b = fst a == fst b

groupWords ws = unzip $ groupPairs $ map g $ filter (not.null) ws
  where g (x:xs) = (x,xs)

getEdges :: [String] -> [(Char,[Char])]
getEdges = Map.assocs . Map.map Set.elems . g
  where
    g = go Map.empty
    merge = Map.unionsWith Set.union
    single x = Map.singleton x Set.empty
    go m [ ] = m
    go m [w] = merge (m:map single w)
    go m ws  = merge (m:es:single z:map g wss)
      where es = Map.map Set.singleton $ Map.fromList $ pairs xs
            (xs,wss) = groupWords ws; z = last xs

graph ws = graphFromEdges $ map lab $ getEdges ws
  where lab (x,xs) = ((),x,xs)

hamiltonian :: Graph -> Maybe [Vertex]
hamiltonian dag 
    | all (`elem` edges dag) (pairs vs) = Just vs
    | otherwise = Nothing
    where vs = topSort dag

alphabet :: [String] -> Maybe String
alphabet ws = fmap (map f) (hamiltonian dag)
  where (dag, nodeFromVertex, vertexFromKey) = graph ws
        f = snd3 . nodeFromVertex where snd3 (_,x,_) = x

byLines f = interact $ unlines . f . lines

showCase (i,m) = concat ["Case #", show i, ": ", s]
  where s = maybe "AMBIGUOUS" (intersperse ' ') m

parse (x:xs) = ws : parse zs where (ws,zs) = splitAt (read x) xs
parse _ = []

main = byLines $ map showCase . zip [1..] . map alphabet . g
  where g (l:ls) = take (read l) (parse ls)
