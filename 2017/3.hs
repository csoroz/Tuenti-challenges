pows2 = iterate (*2) 1 :: [Integer]

cards p = length $ takeWhile (<=p) pows2

byLines f = interact $ unlines . f . lines

printCase (i,n) = concat ["Case #",show i,": ",show n]

main = byLines $ map printCase . zip [1..] . map (cards . read) . g
  where g (l:ls) = take t ls where t = read l
