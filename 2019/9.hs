-- [NOTE] For UTF-8 stdin on Windows cmd: chcp 65001
{-# LANGUAGE LambdaCase,NoMonomorphismRestriction #-}
-- import qualified Math.Combinatorics.Multiset as M -- cabal install multiset-comb
-- import qualified Math.Combinat.Permutations as C -- cabal install combinat
-- import qualified Data.NonEmpty as N -- cabal install non-empty
import Data.List.Split (splitOn)
import Data.List
import Data.Char
import Data.Maybe

-- Kanji numerals (一二三四五六七八九十百千万)
----------------- 1 2 3 4 5 6 7 8 9 X C M T

data Op = Plus | Minus | Times

instance Show Op where
    show Plus  = "+"
    show Minus = "-"
    show Times = "*"

eval Plus  = (+)
eval Minus = (-)
eval Times = (*)

kanji :: Char -> Int
kanji = \case
    '一' -> 1
    '二' -> 2
    '三' -> 3
    '四' -> 4
    '五' -> 5
    '六' -> 6
    '七' -> 7
    '八' -> 8
    '九' -> 9
    '十' -> 10
    '百' -> 100
    '千' -> 1000
    '万' -> 10000

kanjiVal :: [Int] -> Int
kanjiVal = \case
    [ ] -> 0
    [x] -> x
    (x:xs)   | x > 9 -> g 1 x xs
    (u:x:xs) | x > 9 -> g u x xs
    where g u x xs = u*x + kanjiVal xs

splitBy p xs = (takeWhile p xs, dropWhile p xs)

classify :: [Int] -> ([Int],[Int],[Int])
classify s = (reverse y, w, v)
  where (v,z) = splitBy (==1) (sort s)
        (w,y) = splitBy (<10) z

-- perms = M.permutations . M.fromList
-- perms = C.permuteMultiset
perms = unique . permutations
unique = map head . group . sort
enclose a z x = a ++ x ++ z

selects :: [a] -> [(a,[a])]
selects xs = init $ zipWith g (inits xs) (tails xs)
                    where g xs (y:ys) = (y,xs++ys)

comb i ys ws = map (concat . flip (zipWith (++)) yss) (perms wss)
  where n = length ys; [yss,wss] = map pad [ys,ws]
        pad xs = take (n+i) (map return xs ++ repeat [])

combine :: ([Int],[Int],[Int]) -> [[Int]]
combine = \case
  (10000:ys,ws,[1,1]) -> map (enclose [1,10000] [1]) (comb 0 ys ws)
  (10000:ys,ws,[1])   -> map ([1,10000]++) (comb 1 ys ws) 
                      ++ map (++[1]) (f 0 ys ws)
  (10000:ys,ws,[])    -> f 1 ys ws
  (ys,ws,[1])         -> map (++[1]) (comb 0 ys ws)
  (ys,ws,[])          -> comb 1 ys ws
  where f i ys ws = concat [map ([x,10000]++) (comb i ys xs) 
                           | (x,xs) <- unique (selects ws)]

combs = map kanjiVal . combine . classify . map kanji
check (o,a,b,c) = (eval o) a b == c

solve :: (String,String,String) -> (Op,Int,Int,Int)
solve (xs,ys,zs) = let [as,bs,cs] = map combs [xs,ys,zs] 
                    in fromJust $ find check $ do
                        o <- [Plus,Minus,Times]
                        a <- as
                        b <- bs
                        c <- cs
                        return (o,a,b,c)

byLines f = interact $ unlines . f . lines

showCase (i,(o,a,b,c)) = concat ["Case #",show i,": ", unwords [x,u,y,"=",z]]
                                where [x,y,z] = map show [a,b,c]; u = show o

parse s = (a,b,c)
  where
    [x,c] = splitOn "=" $ filter (not.isSpace) s
    [a,b] = splitOn "OPERATOR" x

-- main = byLines $ map showCase . zip [1..] . map solve . rep 8 . map parse . g
main = byLines $ map showCase . zip [1..] . map solve . map parse . g
  where g (l:ls) = take (read l) ls; rep n = concat . replicate n
