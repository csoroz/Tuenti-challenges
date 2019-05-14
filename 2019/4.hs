import Data.Char
import Data.List
import Data.Ratio

lcms = foldl1' lcm

compress = map count . group . sort
  where count xs = (head xs, genericLength xs)

solve :: [Integer] -> Rational
solve as = n % m
  where (n,m) = foldl f (0,0) ys
                where f (n,m) (a,k) = (n + kx, m + div kx a)
                                      where kx = k*x
                      x = lcms $ map (uncurry (*)) ys
                      ys = compress as

byLines f = interact $ unlines . f . lines

showCase (i,x) = concat ["Case #",show i,": ", g x]
  where g = map f . filter (not . isSpace) . show
        f '%' = '/'
        f  x  =  x

parse (x:s:xs) = take (read x) (map read $ words s) : parse xs
parse _ = []

main = byLines $ map showCase . zip [1..] . map solve . g
  where g (l:ls) = take (read l) (parse ls)
