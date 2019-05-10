-- NOTE: For UTF-8 stdin on Windows cmd: chcp 65001
{-# LANGUAGE LambdaCase #-}
import Data.Char
import Data.List
import Data.List.Split hiding (oneOf)
import Data.Maybe
import RegExp

-- Kanji numerals (一二三四五六七八九十百千万)

tens3 = "十百千"
tens4 = "十百千万"

classify :: String -> (String,String,String)
classify s = (v,w,y)
  where (y,z) = partition (`elem` tens4) s
        (v,w) = partition (== '一') (nub z)

oneOf :: String -> Rexp Char
oneOf = foldr (:|) Phi . map Single

kanjiRexp :: (String,String,String) -> Rexp Char
kanjiRexp (v,w,y) = o (u :. g t)
                 :. o (a :. g m)
                 :. o (a :. g c)
                 :. o (a :. g x)
                 :. o u
                 where
                    [x,c,m,t] = tens4
                    u = oneOf (v ++ w)
                    a = o (oneOf w)
                    o = (:| Nil)
                    g a = if elem a y then Single a else Phi

kanji1 :: Char -> Int
kanji1 = \case
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

kanjiN :: String -> Int
kanjiN = \case
    [ ] -> 0
    [u] -> kanji1 u
    (x:xs)   | elem x tens3 -> g '一' x xs
    (u:x:xs) | elem x tens4 -> g u x xs
    where g a b xs = kanji1 a * kanji1 b + kanjiN xs

data Op = Plus | Minus | Times

eval Plus  = (+)
eval Minus = (-)
eval Times = (*)

instance Show Op where
    show Plus  = "+"
    show Minus = "-"
    show Times = "*"

check (o,a,b,c) = (eval o) a b == c

solve :: (String,String,String) -> (Op,Int,Int,Int)
solve (as,bs,cs) = fromJust $ find check $ do
                    o <- [Plus,Minus,Times]
                    a <- combs as
                    b <- combs bs
                    c <- combs cs
                    return (o,a,b,c)
  where
    matches x = let t = sort x in filter ((t==).sort) (kanjis x)
    kanjis = enum . kanjiRexp . classify
    combs = map kanjiN . matches

byLines f = interact $ unlines . f . lines

showCase (i,(op,a,b,c)) = concat ["Case #",show i,": ", unwords [show a, show op, show b,"=",show c]]

parse s = (a,b,c)
  where
    [x,c] = splitOn "=" $ filter (not.isSpace) s
    [a,b] = splitOn "OPERATOR" x

main = byLines $ map showCase . zip [1..] . map (solve . parse) . g
  where g (l:ls) = take (read l) ls
