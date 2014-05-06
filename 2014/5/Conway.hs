import Data.Array.Unboxed
import Data.Array.Base
import Data.List
 
type Grid = UArray (Int,Int) Bool
 
life :: Grid -> Grid
life a = listArray b $ map alive (range b)
  where alive p = v && n == 2 || n == 3
          where n = count $ map (a!) $ neighbours p
                count = length . filter id
                v = a!p
        b = bounds a
        neighbours (x,y) = filter (inRange b) $ map go
                [(-1,-1),(0,-1),(1,-1),
                 (-1, 0),       (1, 0),
                 (-1, 1),(0, 1),(1, 1)]
            where go (dx,dy) = (x+dx,y+dy)

loop :: Eq a => [a] -> (Int,Int)
loop = go []
  where
    go vs (x:xs) 
        | null zs   = go (x:vs) xs
        | otherwise = (length zs - 1, length ys + 1)
        where (ys,zs) = break (x==) vs

solve = unpair . loop . iterate life
out = putStrLn . unwords . map show
unpair (a,b) = [a,b]

main = out . solve . listArray ((1,1),(8,8)) . g (8,8) . lines =<< getContents
  where g (m,n) = map ('X'==) . concat . map (take m) . take n
