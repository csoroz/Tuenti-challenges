
count 0 = 0
count 1 = 1
count 2 = 1
count n = (n - 1) `div` 2

printCase (i,n) = putStrLn $ concat $ ["Case #",show i,": ",show n]

main = mapM_ printCase . zip [1..] . map (count . read) . g . lines =<< getContents
  where g (l:ls) = take t ls where t = read l
