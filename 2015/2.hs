import Data.List

ldp = ldpf primes

ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise = ldpf ps n

prime n = case compare n 2 of
            LT -> False
            EQ -> True
            GT -> ldp n == n

primes = 2 : filter prime [3,5..]

factors = go primes
  where go ps@(p:zs) n
          | p^2 > n   = [n]
          | r == 0    =  p : go ps m
          | otherwise =      go zs n
          where (m,r) = quotRem n p

semiprime :: Int -> Bool
semiprime = check . factors
  where check [_,_] = True
        check  _    = False

count :: (Int,Int) -> Int
count (a,b) = sum $ map (fromEnum . semiprime) [a..b]

pair [a,b] = (a,b)

main = mapM_ (print . count . f) . g . lines =<< getContents
  where g (l:ls) = take t ls where t = read l
        f = pair . map read . take 2 . words
