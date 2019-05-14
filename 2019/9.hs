-- [NOTE] For UTF-8 stdin on Windows cmd: chcp 65001
{-# LANGUAGE LambdaCase,NoMonomorphismRestriction #-}
--import qualified Math.Combinatorics.Multiset as M -- cabal install multiset-comb
--import qualified Math.Combinat.Permutations as C -- cabal install combinat
--import qualified Data.NonEmpty as N -- cabal install non-empty
import Data.List.Split (splitOn)
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad

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

selects xs = init $ zipWith g (inits xs) (tails xs)
                    where g xs (y:ys) = (y,xs++ys)

between a z x = a ++ x ++ z

unique = map head . group . sort

-- perms = M.permutations . M.fromList
-- perms = C.permuteMultiset
perms = unique . permutations

comb i ys ws = map (concat . flip (zipWith (++)) yss) (perms wss)
  where n = length ys; yss = pad ys; wss = pad ws
        pad xs = take (n+i) (map return xs ++ repeat [])

combine :: ([Int],[Int],[Int]) -> [[Int]]
combine = unique . g
  where
    g (10000:ys,ws,[1,1]) = map (between [1,10000] [1]) (comb 0 ys ws)
    g (10000:ys,ws,[1]) = map ([1,10000]++) (comb 1 ys ws) ++ map (++[1]) (f 0 ys ws)
    g (10000:ys,ws,[]) = f 1 ys ws
    g (ys,ws,[1]) = map (++[1]) (comb 0 ys ws)
    g (ys,ws,[]) = comb 1 ys ws
    f i ys ws = concat [map ([x,10000]++) (comb i ys xs) | (x,xs) <- selects ws]

combs = map kanjiVal . combine . classify . map kanji

check (o,a,b,c) = (eval o) a b == c

solve :: (String,String,String) -> (Op,Int,Int,Int)
solve (as,bs,cs) = fromJust $ find check $ do
                    o <- [Plus,Minus,Times]
                    a <- combs as
                    b <- combs bs
                    c <- combs cs
                    return (o,a,b,c)

byLines f = interact $ unlines . f . lines

showCase (i,(o,a,b,c)) = concat ["Case #",show i,": ", unwords [show a, show o, show b,"=",show c]]

parse s = (a,b,c)
  where
    [x,c] = splitOn "=" $ filter (not.isSpace) s
    [a,b] = splitOn "OPERATOR" x

-- main = byLines $ map showCase . zip [1..] . map solve . rep 10 . map parse . g
main = byLines $ map showCase . zip [1..] . map solve . map parse . g
  where g (l:ls) = take (read l) ls; rep n = concat . replicate n
