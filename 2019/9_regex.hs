{-# LANGUAGE LambdaCase #-}
import Data.List.Split hiding (oneOf)
import Data.List
import Data.Char
import Data.Maybe
import RegExp

-- [NOTE] For UTF-8 stdin on Windows cmd: chcp 65001

-- Kanji numerals (一二三四五六七八九十百千万)

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
classify s = (reverse y, nub w, nub v)
  where (v,z) = splitBy (==1) (sort s)
        (w,y) = splitBy (<10) z

-- Regexp

alt, str :: [Rexp z] -> Rexp z
alt = foldr (:|) Phi
str = foldr (:.) Nil

oneOf :: [z] -> Rexp z
oneOf = alt . map Single

-- tens = reverse [10,100,1000,10000]
-- kanjiRexp (v,w,y) = str (zipWith g [u,a,a,a] tens) :. o u

kanjiRexp :: ([Int],[Int],[Int]) -> Rexp Int
kanjiRexp (y,w,v) = g u 10000
                 :. g a 1000
                 :. g a 100
                 :. g a 10
                 :. o u
                 where
                    u = oneOf (v ++ w)
                    a = o (oneOf w)
                    g a b | elem b y = a :. Single b
                    g _ _ = Nil
                    o = (:| Nil)

solve :: (String,String,String) -> (Op,Int,Int,Int)
solve (as,bs,cs) = fromJust $ find check $ do
                    o <- [Plus,Minus,Times]
                    a <- combs as
                    b <- combs bs
                    c <- combs cs
                    return (o,a,b,c)
  where
    matches x = let t = sort x in filter ((t==).sort) (kanjis x)
    combs = map kanjiVal . matches . map kanji
    kanjis = enum . kanjiRexp . classify
    check (o,a,b,c) = (eval o) a b == c

byLines f = interact $ unlines . f . lines

showCase (i,(o,a,b,c)) = concat ["Case #",show i,": ", unwords [x,u,y,"=",z]]
                                where [x,y,z] = map show [a,b,c]; u = show o

parse s = (a,b,c)
  where
    [x,c] = splitOn "=" $ filter (not.isSpace) s
    [a,b] = splitOn "OPERATOR" x

main = byLines $ map showCase . zip [1..] . map (solve . parse) . g
  where g (l:ls) = take (read l) ls
