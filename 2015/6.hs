import Control.Applicative
import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.Text.Read
import Data.Either
import Data.Vector (Vector,fromList,unsafeIndex)
import qualified Data.Vector.Unboxed as U
import Data.Word

type I = Word64
type A = Vector (U.Vector I)

infixl 9 #
a # (y,x) = U.unsafeIndex (unsafeIndex a y) x

toArray = fromList . map (U.force . U.fromList)

accumulate ::  Int -> [[I]] -> A
accumulate n = toArray . scanl (zipWith (+)) z . map (scanl (+) 0)
  where z = replicate (n+1) 0

query :: A -> [Int] -> I
query a [y0,x0,y1,x1,k] = maximum
    [airscrew (y,x) | y <- [y0+k..y1-k], x <- [x0+k..x1-k]]
  where
    airscrew (y,x) = square (y-1,x-1) + square (y+k,x+k)
    square (y,x) = a # (u,r) - a # (u,l)
                 + a # (b,l) - a # (b,r)
      where [b,u,l,r] = map (+1) [y-k,y,x-k,x]

readI :: Integral a => Text -> a
readI = fst . g . decimal where g (Right x) = x

printCase (i,y) = putStrLn $ concat $ ["Case ",show i,": ",show y]

main = withFile "sheet.data" ReadMode $ \h -> do
        a <- uncurry accumulate . arr . T.lines <$> Tio.hGetContents h
        zs <- map (query a . g 5) . f . T.lines <$> Tio.getContents
        mapM_ printCase $ zip [1..] zs
  where f (x:xs) = take n xs where n = readI x
        g n = map readI . take n . T.words
        arr (x:xs) = (n, map (g n) $ take m xs)
          where [m,n] = g 2 x
