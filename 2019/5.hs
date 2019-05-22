import Data.Map (Map,(!))
import qualified Data.Map.Strict as Map
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.Tuple
import Data.Char

typewriter :: UArray (Int,Int) Char
typewriter = U.listArray ((0,0),(3,9))
             $ concat ["1234567890"
                      ,"QWERTYUIOP"
                      ,"ASDFGHJKL;"
                      ,"ZXCVBNM,.-"]

keys :: Map Char (Int,Int)
keys = Map.fromList $ map swap (U.assocs typewriter)

bound (x,y) = (mod x 4, mod y 10)

infixl 6 !-!, !+!
(a,b) !-! (x,y) = (a - x, b - y)
(a,b) !+! (x,y) = (a + x, b + y)

decrypt (a,s) = map f s
  where
    f x = if isSpace x then x else g x
    g x = typewriter U.! bound (d !+! keys!x)
    d = keys!a !-! keys!(last s)

byLines f = interact $ unlines . f . lines

showCase (i,s) = concat ["Case #",show i,": ",s]

parse (x:s:xs) = (head x, s) : parse xs
parse _ = []

main = byLines $ map showCase . zip [1..] . map decrypt . g
  where g (l:ls) = take (read l) (parse ls)
