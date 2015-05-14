import qualified Data.Map.Strict as M
import Data.Array
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Function
import Data.Ord

type I = Int

combs :: Int -> [a] -> [[a]]
combs 0 _ = [[]]
combs n l = [x:ys | x:xs <- tails l, ys <- combs (n-1) xs]

pairs :: [a] -> [(a,a)]
pairs = map pair . combs 2 where pair [a,b] = (a,b)

edges :: [a] -> [(a,a)]
edges vs = ws ++ map swap ws where ws = pairs vs

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

data G = G { answers :: [Bool], friends :: [I] } deriving Show
type Graph = Array I G

connected :: Graph -> I -> [I]
connected g i = go [i] $ friends (g!i)
  where go as [] = as
        go as ys = go xs $ unionFriends g ys \\ xs
            where xs = union as ys

unionFriends g = foldr union [] . map (friends.(g!))

score :: Graph -> I -> Int
score g i = go (g!i)
  where
    go x = sum
        [ check naughty 7
        , 3 * heroes
        , 6 * suits
        , check cats 4
        , 5 * shopping 
        ]
      where
        ys = friends x
        check p k = if p then k else 0
        ans w = (!!w) . answers . (g!)
        naughty = answers x !! 0
        heroes = length $ filter (ans 1) ys
        suits = length $ filter (ans 2) $ (\\i:ys) $ unionFriends g ys
        cats = any likes ys 
          where likes k = ans 3 k && (not $ any (ans 3) $ others k)
                others k = friends (g!k) \\ [i]
        shopping = length $ filter (ans 4) $ [a..b] \\ connected g i
          where (a,b) = bounds g

solve :: (Graph,I) -> Int
solve (g,n) = maximum $ map (score g) [1..n]

prepare :: (I, [(String,[Bool])], [[String]]) -> (Graph,I)
prepare (n, girls,friends) = (g, n)
  where
    (names,ans) = unzip girls
    m = M.fromList $ zip names [1..]
    look = fromJust . flip M.lookup m
    ys = map (map look) friends
    ws = foldr union [] $ map edges ys
    index = map (onFst head . unzip) . groupOn fst
    g = toArray $ map toG $ zipIndex [] ans $ index ws
      where toG (as,ys) = G as ys

main = print . solve . prepare . g . lines =<< getContents
  where g (l:ls) = (n, map parse xs, map words $ take m ys)
          where [n,m] = map read $ take 2 $ words l
                (xs,ys) = splitAt n ls
                parse = f . take 6 . words
                  where f (x:xs) = (x, map ans xs)
                          where ans "Y" = True
                                ans "N" = False
