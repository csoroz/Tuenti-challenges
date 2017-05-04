{-# LANGUAGE BangPatterns #-}
import Data.List.Split

parse = reverse . splitOn "-"

combos = map parse
  ["L-LD-D-RD-R-P"
  ,     "D-RD-R-P"
  ,     "R-D-RD-P"
  ,     "D-LD-L-K"
  ,"R-RD-D-LD-L-K"]

count :: String -> Int
count = go 0 . ("":) . parse
  where
    go !n [] = n
    go !n xs@(_:ys) = go (n+m) ys
      where
        m = fromEnum $ any (match xs) combos
        match [] _ = False
        match x  p = g (/=) x p
        eq _ [] = True
        eq [] _ = False
        eq x  p = g (==) x p
        g e (x:xs) (p:ps) = e x p && eq xs ps

printCase (i,n) = putStrLn $ concat $ ["Case #",show i,": ",show n]

main = mapM_ printCase . zip [1..] . map count . g . lines =<< getContents
  where g (l:ls) = take t ls where t = read l
