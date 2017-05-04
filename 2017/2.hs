{-# LANGUAGE BangPatterns #-}

bowling  :: [Int] -> [Int]
bowling = go 10 0
  where
    strike (a:b:xs) = a + b
    go 0  _ _ = []
    go k !n (10:xs) = m : go (k-1) m xs
      where m = n + 10 + strike xs
    go k !n (a:b:xs) = m : go (k-1) m xs
      where m = n + x + if x == 10 then head xs else 0
              where x = a + b

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = ys : chop n zs where (ys,zs) = splitAt n xs

byLines f = unlines . f . lines

printCase (i,xs) = concat ["Case #",show i,": ",unwords $ map show xs]

main = interact $ byLines $ map printCase . zip [1..] . map (bowling . f) . g
  where g (l:ls) = take t (chop 2 ls) where t = read l
        f [n,xs] = take (read n) $ map read $ words xs
