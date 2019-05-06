import Data.List
import Data.List.Split
import Data.Maybe

type G a = [(a,[a])]
type P a = [[(a,a)]]

connect :: Eq a => a -> a -> G a -> P a
connect x y g
    | x == y    = [[]]
    | otherwise = [(x,t):p | t <- f x, p <- connect t y g]
    where f = fromJust . flip lookup g

solve = length . connect "Galactica" "New Earth"

byLines f = interact $ unlines . f . lines

showCase (i,x) = concat ["Case #",show i,": ", show x]

parse [] = []
parse (x:xs) = map g ys : parse zs
  where
    (ys,zs) = splitAt (read x) xs
    g s = (v, splitOn "," (head w))
      where (v:w) = splitOn ":" s

main = byLines $ map showCase . zip [1..] . map solve . g
  where g (l:ls) = take (read l) (parse ls)
