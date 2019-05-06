import Data.Char
import Data.List
import Data.Ratio

compress = map count . group . sort
  where count xs = (head xs, genericLength xs)

lcms = foldl1' lcm

solve :: [Integer] -> Rational
solve as = n % m
  where
    (n,m) = foldr f (0,0) ys
            where f (a,k) (n,m) = (n + kx, m + div kx a)
                                  where kx = k*x
    ys = compress as
    x = lcms $ map (uncurry (*)) ys

byLines f = interact $ unlines . f . lines

showCase (i,x) = concat ["Case #",show i,": ", g x]
  where g = map f . filter (not . isSpace) . show
        f '%' = '/'
        f  x  =  x

parse (x:s:xs) = take (read x) (map read $ words s) : parse xs
parse _ = []

main = byLines $ map showCase . zip [1..] . map solve . g
  where g (l:ls) = take (read l) (parse ls)
