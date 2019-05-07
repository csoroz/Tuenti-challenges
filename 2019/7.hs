import Data.Array.Unboxed
import Data.Char
import Data.Int
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

rotate n xs = take xs (drop n (cycle xs))
        where take = zipWith (flip const)

decompose :: (Int,Int) -> Int -> Int -> Maybe [Int]
decompose (a,b) n x
  | y > n*b = Nothing
  | otherwise = Just (replicate (n-i-1) a ++ d:replicate i b)
  where above m = head . dropWhile (<m) . iterate (+256)
        y = above (n*a) x
        (i,d) = g 0
          where g i | d >= a && b >= d = (i,d)
                    | otherwise = g (i+1)
                    where d = y - ((n-i-1)*a + i*b)

update a f i = a // [(i, f (a!i))]

hash :: String -> UArray Int Int8
hash = foldl f (listArray (0,15) (replicate 16 0)) . zip [0..]
  where f a (i,x) = update a h (mod i 16)
          where h u = u + fromIntegral (ord x)

solve :: (String,String) -> String
solve (xs,ys) = map chr $ concat $ transpose $ head $ catMaybes $ map go [0..]
  where [as,zs] = [a++"---","---"++z] where [a,z] = splitOn "------" ys
        [x,a,z] = map (map fromIntegral . elems . hash) [xs,as,zs]
        [x',a'] = map (rotate m) [x,a]
        t = zipWith (-) x' a'
        l = length as
        m = mod l 16
        go i = sequence $ map (uncurry $ decompose (48,122)) (zip ns ds)
          where
            (n,k) = divMod i 16
            r = mod (16-l-k+m) 16
            ds = zipWith (-) t (rotate r z)
            ns = replicate k (n+1) ++ replicate (16-k) n

byLines f = interact $ unlines . f . lines

showCase (i,s) = concat ["Case #",show i,": ",s]

parse (m:xs) = (concat a, concat b) : parse zs
  where (a,n:ys) = splitAt (read m) xs
        (b,zs)   = splitAt (read n) ys
parse _ = []

main = byLines $ map showCase . zip [1..] . map solve . g
  where g (l:ls) = take (read l) (parse ls)
