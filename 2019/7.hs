import Data.Array.Unboxed
import Data.Char
import Data.Int
import Data.List
import Data.List.Split
import Data.Maybe

rotate n xs = zipWith const (drop n (cycle xs)) xs

decompose :: (Int,Int) -> (Int,Int) -> Maybe [Int]
decompose (a,b) (n,x)
  | y > n*b = Nothing
  | otherwise = Just (replicate (n-i-1) a ++ v:replicate i b)
  where above m = head . dropWhile (<m) . iterate (+256)
        y = above (n*a) x
        (i,v) = g 0
          where g i | a <= v && v <= b = (i,v)
                    | otherwise = g (i+1)
                    where v = y - (n-i-1)*a - i*b

hN = 16

hash :: String -> UArray Int Int8
hash = foldl f zeroes . zip [0..]
  where zeroes = listArray (0,hN-1) (replicate hN 0)
        f a (i,x) = update (mod i hN)
          where update i = a // [(i, h (a!i))]
                h u = u + fromIntegral (ord x)

solve :: (String,String) -> String
solve (xs,ys) = columns $ head $ catMaybes $ map crack [0..]
  where columns = map chr . concat . transpose
        [as,zs] = [a++"---","---"++z] where [a,z] = splitOn "------" ys
        [x,a,z] = map (map fromIntegral . elems . hash) [xs,as,zs]
        [x',a'] = map (rotate m) [x,a]
        t = zipWith (-) x' a'
        l = length as
        m = mod l hN
        crack i = sequence $ map (decompose (48,122)) (zip ns ds)
          where
            (n,k) = divMod i hN
            r = mod (hN-l-k+m) hN
            ds = zipWith (-) t (rotate r z)
            ns = replicate k (n+1) ++ replicate (hN-k) n

byLines f = interact $ unlines . f . lines

showCase (i,s) = concat ["Case #",show i,": ",s]

parse (m:xs) = (concat a, concat b) : parse zs
  where (a,n:ys) = splitAt (read m) xs
        (b,zs)   = splitAt (read n) ys
parse _ = []

main = byLines $ map showCase . zip [1..] . map solve . g
  where g (l:ls) = take (read l) (parse ls)
