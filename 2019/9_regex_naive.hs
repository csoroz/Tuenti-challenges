{-# LANGUAGE LambdaCase #-}
import Data.List.Split hiding (oneOf)
import Data.List
import Data.Char
import Data.Maybe
import Data.Ord
import RegExp

-- NOTE: For UTF-8 stdin on Windows cmd: chcp 65001

-- Kanji numerals (一二三四五六七八九十百千万)

data Op = Plus | Minus | Times

instance Show Op where
    show Plus  = "+"
    show Minus = "-"
    show Times = "*"

eval Plus  = (+)
eval Minus = (-)
eval Times = (*)

numKanji :: Char -> Int
numKanji = \case
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

fromKanji :: String -> Int
fromKanji = \case
    [ ] -> 0
    [u] -> numKanji u
    (x:xs)   | elem x "十百千"   -> g '一' x xs
    (u:x:xs) | elem x "十百千万" -> g u x xs
    where g a b xs = numKanji a * numKanji b + fromKanji xs

-- Regexp

oneOf = foldl1 (:|) . map Single

kanjiRexp = o (u :. Single t)
         :. o (a :. Single m)
         :. o (a :. Single c)
         :. o (a :. Single x)
         :. o u
         where
            units = "一二三四五六七八九"
            [x,c,m,t] = "十百千万"
            u = oneOf units
            a = o (oneOf (tail units))
            o = (:| Nil)

solve :: (String,String,String) -> (Op,Int,Int,Int)
solve (as,bs,cs) = fromJust $ find check $ do
                    o <- [Plus,Minus,Times]
                    a <- combs as
                    b <- combs bs
                    c <- combs cs
                    return (o,a,b,c)
  where
    matches x = let t = sort x in filter ((t==).sort) kanjis
    combs = map fromKanji . matches
    kanjis = tail (enum kanjiRexp)
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
