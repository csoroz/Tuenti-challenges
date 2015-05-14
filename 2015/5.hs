import qualified Data.Map.Strict as M
import Data.Array
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

type I = Int
type Gold = Int
data Route = R { node :: I, routeCost :: Gold } deriving Show
data Node  = N { cost :: Gold, routes :: [Route] } deriving Show
type Graph = Array I Node

data Ship = Ship 
          { number  :: Int
          , gold    :: Gold
          , current :: I
          , visited :: [I]
          } deriving Show

onFst f (a,b) = (f a,b)
onSnd f (a,b) = (a,f b)

groupOn g = groupBy ((==) `on` g) . sortBy (comparing g)

toArray xs = listArray (1, length xs) xs

zipIndex :: b -> [a] -> [(I,b)] -> [(a,b)]
zipIndex b = go . zip [1..]
  where go [] _  = []
        go ((_,x):xs) [] = (x,b) : go xs []
        go ((i,x):xs) yys@((j,y):ys)
            | i == j     = (x,y) : go xs ys
            | otherwise  = (x,b) : go xs yys

avails :: Graph -> Ship -> [Route]
avails g x = filter f $ routes $ g ! current x
  where f = (`notElem` visited x) . node

move :: Ship -> I -> Ship
move x@(Ship _ _ i vs) to = x {visited = i:vs, current = to}

moveOther :: Graph -> Ship -> Maybe Ship
moveOther g x = return . move x . node =<< select
  where
    f = if odd (number x) then minimum else maximum
    select = case avails g x of
        [] -> Nothing
        ys -> find ((m==).routeCost) ys
          where m = f (map routeCost ys)

type A = Array I [Gold]

arrivals :: Graph -> Int -> [Ship] -> [A]
arrivals g n = map arriv . ([]:) . tail . iterate step
  where
    step = catMaybes . map (moveOther g)
    zero = listArray (1,n) (replicate n [])
    arriv = foldr f zero
      where f x a = a // [(i, gold x : a!i)] 
                    where i = current x

data G = G Graph I Ship [A] 

prepare :: ([(String,Gold)], [(String,String,Gold)], [(Int,Gold,String)]) -> G
prepare (nodes,paths,ships) = G g r x $ arrivals g n xs
  where
    n = length nodes
    (names,costs) = unzip nodes
    m = M.fromList $ zip names [1..]
    look = fromJust . flip M.lookup m
    r = look "Raftel"
    rs = map f paths where f (a,b,x) = (look a,(look b,x))
    (x:xs) = map f ships where f (n,k,a) = Ship n k i [] where i = look a
    indexRoutes = map (onFst head . unzip) . groupOn fst
    g = listArray (1,n) $ map toN $ zipIndex [] costs $ indexRoutes rs
      where toN (x,xs) = N x (map toR xs) where toR (i,ys) = R i ys

run :: G -> Int
run (G g r x as) = go x as
  where
    go x@(Ship _ k i _) (a:as)
      | i == r = k
      | not (null $ a!r) = 0
      | otherwise = maximum (pillage:travels)
      where
        pay a b = max 0 $ a - b
        money = pay k $ sum (a!i)
        pillage = go y as where y = x{gold = money + 10}
        travels | money > 0 = map tr $ avails g x
                | otherwise = []
          where tr (R n m) = go (move y n) as
                  where y = x{gold = pay money $ m + cost (g!n)}

parse [nodes,paths,ships] = (map f nodes, map g paths, map h ships)
  where
    f xs = (n,read c) where [n,c] = take 2 $ words xs
    g xs = (a,b,read c) where [a,b,c] = take 3 $ words xs
    h xs = (read i,read k,x) where [i,_,k,x] = take 4 $ words xs

main = print . run . prepare . g . lines =<< getContents
  where g = parse . take 3 . unfoldr f
          where
            f (x:xs) = Just (splitAt n xs) where n = read x
            f []     = Nothing
