import Data.Map (Map,(!))
import qualified Data.Map.Strict as Map
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Data.Tuple
import Data.Char

typewriter :: UArray (Int,Int) Char
typewriter = UA.listArray ((0,0),(3,9)) $ concat
                        ["1234567890"
                        ,"QWERTYUIOP"
                        ,"ASDFGHJKL;"
                        ,"ZXCVBNM,.-"]

keys :: Map Char (Int,Int)
keys = Map.fromList $ map swap (UA.assocs typewriter)

bound (x,y) = (mod x 4, mod y 10)

infixl 6 <->, <+>
(a,b) <-> (x,y) = (a - x, b - y)
(a,b) <+> (x,y) = (a + x, b + y)

decrypt (a,s) = map g s
  where
    g x | isSpace x = x
        | otherwise = typewriter UA.! bound (d <+> keys ! x)
    x = keys ! a
    z = keys ! (last s)
    d = x <-> z

byLines f = interact $ unlines . f . lines

showCase (i,s) = concat ["Case #",show i,": ",s]

parse (x:s:xs) = (head x, s) : parse xs
parse _ = []

main = byLines $ map showCase . zip [1..] . map decrypt . g
  where g (l:ls) = take (read l) (parse ls)
