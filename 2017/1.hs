
divUp a b = n + if m == 0 then 0 else 1
  where (n,m) = divMod a b

pizzas :: [Int] -> Int
pizzas = flip divUp 8 . sum

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = ys : chop n zs where (ys,zs) = splitAt n xs

byLines f = interact $ unlines . f . lines

showCase (i,n) = concat ["Case #",show i,": ",show n]

main = byLines $ map showCase . zip [1..] . map (pizzas . f) . g
  where g (l:ls) = take t (chop 2 ls) where t = read l
        f [n,xs] = take (read n) $ map read $ words xs
