-- [NOTE] For UTF-8 stdin on Windows cmd: chcp 65001
{-# LANGUAGE LambdaCase,NoMonomorphismRestriction #-}
import Data.List.Split (splitOn)
import Data.List
import Data.Char
import Data.Maybe

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
classify s = (v, w, reverse y)
  where (v,z) = splitBy (==1) (sort s)
        (w,y) = splitBy (<10) z

splits xs = map (flip splitAt xs) [0..n]
  where n = length xs

selects xs = [(z,ys++zs) | (ys,z:zs) <- splits xs]

unique = map head . group . sort

between a z x = a ++ x ++ z

comb i xs ys = map (concat . flip (zipWith (++)) yss) (permutations xss)
  where n = length ys; yss = pad ys; xss = pad xs
        pad xs = take (n+i) (map return xs ++ repeat [])

combine :: ([Int],[Int],[Int]) -> [[Int]]
combine = unique . g
  where
    g ([1,1],ws,10000:ys) = map (between [1,10000] [1]) (comb 0 ws ys)
    g ([1],ws,10000:ys) = 
        map ([1,10000]++) (comb 1 ws ys) ++ map (++[1]) (f 0 ws ys)
    g ([1],ws,ys) = map (++[1]) (comb 0 ws ys)
    g ([],ws,10000:ys) = f 1 ws ys
    g ([],ws,ys) = comb 1 ws ys
    f i ws ys = concat [map ([x,10000]++) (comb i xs ys) | (x,xs) <- selects ws]

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

main = byLines $ map showCase . zip [1..] . map (solve . parse) . g
  where g (l:ls) = take (read l) ls
