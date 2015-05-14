import Data.Word

count :: Word -> Word
count n = m + r where (m,r) = divMod n 2

main = mapM_ (print . count . read) . g . lines =<< getContents
  where g (l:ls) = take t ls where t = read l
