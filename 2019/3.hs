import Data.List

data Folds = L | R | T | B deriving (Read,Show)
type Punch = (Int,Int)

solve :: (Int,Int,[Folds],[Punch]) -> [Punch]
solve (w,h,fs,ps) = sort $ punches $ foldl step (0,w,0,h,ps) fs
  where
    punches (l,_,t,_,ps) = map (delta (l,t)) ps
      where delta (dx,dy) (x,y) = (x-dx,y-dy)
    step (l,r,t,b,ps) f = case f of
        L -> (l-x,r,t,b, g dX l)
        R -> (l,r+x,t,b, g dX r)
        T -> (l,r,t-y,b, g dY t)
        B -> (l,r,t,b+y, g dY b)
        where (x,y) = (r-l, b-t)
              g d a = ps ++ map (d a) ps
              dX a (x,y) = (2*a-x-1,y)
              dY a (x,y) = (x,2*a-y-1)

byLines f = interact $ unlines . f . lines

showCase (i,xs) = concat ["Case #",show i,":"] : map f xs
  where f = unwords . map show . unpair

pair [a,b] = (a,b)
unpair (a,b) = [a,b]

parse [] = []
parse (x:xs) = (read w, read h, map read fs, map g ps) : parse zs
  where
    [w,h,f,p] = take 4 $ words x
    (fs,ys) = splitAt (read f) xs
    (ps,zs) = splitAt (read p) ys
    g = pair . take 2 . map read . words

main = byLines $ concat . map showCase . zip [1..] . map solve . g
    where g (l:ls) = take (read l) (parse ls)
