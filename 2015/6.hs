import Control.Applicative
import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.Text.Read
import Data.Either
import qualified Data.Vector.Unboxed as U
import Data.List
import Data.Word

type I = Word64
data A = A Int (U.Vector I)

infixl 9 #
(A n v) # (y,x) = U.unsafeIndex v (n*y + x)
{-# INLINE (#) #-}

toArray n = A n . U.force . U.fromList . concat

sum' = foldl' (+) 0
maximum' = foldl1' max
accumulate = scanl (+)

query :: A -> [Int] -> I
query a [y0,x0,y1,x1,k] = maximum' $ concat $
    zipWith accumulate (accumulate z dx0) dya
  where
    z = d0 (y0+k) (x0+k)
    ix = [x0+k+1..x1-k]
    iy = [y0+k+1..y1-k]
    dx0 = [dx (y0+k) x | x <- ix]
    dy0 = [dy y (x0+k) | y <- iy]
    dya = transpose $ map dyx $ zip iy dy0
      where dyx (y,z) = accumulate z [d y x | x <- ix]
              where d y x = a#(y+k,x+k)   - a#(y,x+k)
                          - a#(y+k,x)     + a#(y,x)
                          + a#(y-1,x-1)   - a#(y-k-1,x-1)
                          - a#(y-1,x-k-1) + a#(y-k-1,x-k-1)
    d0 y x = sum' [a#(y+i,x+j) + a#(y-i,x-j) | i <- [1..k], j <- [1..k]]
    dx y x = sum' [d (i,x+k) + d (-i,x-1) | i <- [1..k]] where d (i,x) = a#(y+i,x) - a#(y+i,x-k)
    dy y x = sum' [d (i,y+k) + d (-i,y-1) | i <- [1..k]] where d (i,y) = a#(y,x+i) - a#(y-k,x+i)

readI :: Integral a => Text -> a
readI = fst . g . decimal where g (Right x) = x

printCase (i,y) = putStrLn $ concat $ ["Case ",show i,": ",show y]

main = withFile "sheet.data" ReadMode $ \h -> do
        a <- uncurry toArray . arr . T.lines <$> Tio.hGetContents h
        zs <- map (query a . g 5) . f . T.lines <$> Tio.getContents
        mapM_ printCase $ zip [1..] zs
  where f (x:xs) = take n xs where n = readI x
        g n = map readI . take n . T.words
        arr (x:xs) = (n, map (g n) $ take m xs)
          where [m,n] = g 2 x
