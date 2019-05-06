
divUp a b = n + if m == 0 then 0 else 1
  where (n,m) = divMod a b

tortillas :: [Int] -> Int
tortillas = sum . map (flip divUp 2)

byLines f = interact $ unlines . f . lines

showCase (i,n) = concat ["Case #",show i,": ",show n]

main = byLines $ map showCase . zip [1..] . map (tortillas . f) . g
  where g (l:ls) = take t ls where t = read l
        f = take 2 . map read . words
