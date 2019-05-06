import qualified Data.Map.Strict as Map
import Data.Array.Unboxed
import Data.Tuple
import Data.Char

typewriter :: UArray (Int,Int) Char
typewriter = listArray ((0,0),(3,9)) $ concat
                ["1234567890"
                ,"QWERTYUIOP"
                ,"ASDFGHJKL;"
                ,"ZXCVBNM,.-"]

keys :: Map.Map Char (Int,Int)
keys = Map.fromList $ map swap (assocs typewriter)

(a,b) <-> (x,y) = (a - x, b - y)
(a,b) <+> (x,y) = (add 4 a x, add 10 b y)
    where add m a b = (a + b) `mod` m

decript (a,s) = map g s
  where
    g x | isSpace x = x
        | otherwise = typewriter ! (d <+> (keys Map.! x))
    x = keys Map.! a
    z = keys Map.! (last s)
    d = x <-> z

byLines f = interact $ unlines . f . lines

showCase (i,s) = concat ["Case #",show i,": ",s]

parse (x:s:xs) = (head x, s) : parse xs
parse _ = []

main = byLines $ map showCase . zip [1..] . map decript . g
  where g (l:ls) = take (read l) (parse ls)
