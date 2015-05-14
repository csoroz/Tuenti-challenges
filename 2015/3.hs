import Control.Applicative
import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.Text.Read
import Data.Either
import Data.Array
import Data.List

ldp = ldpf primes

ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise = ldpf ps n

prime n = case compare n 2 of
            LT -> False
            EQ -> True
            GT -> ldp n == n

primes = 2 : filter prime [3,5..]

factors = go primes
  where go ps@(p:zs) n
          | p^2 > n   = [n]
          | r == 0    =  p : go ps m
          | otherwise =      go zs n
          where (m,r) = quotRem n p

type Factor = (Integer,Int)

factorize :: Integer -> [Factor]
factorize = map count . group . factors
  where count xs = (head xs, length xs)

toArray xs = listArray (0, length xs - 1) xs

accumulate :: Num a => [a] -> Array Int a
accumulate = toArray . scanl (+) 0

data V = V [Int] deriving Show

instance Num V where
    V xs + V ys = V $ zipWith (+) xs ys
    negate (V xs) = V $ map negate xs
    fromInteger = V . repeat . fromInteger
    signum = undefined
    abs = undefined
    (*) = undefined

primesV = take 25 primes

vectorize :: [Factor] -> V
vectorize = V . reverse . go [] primesV
  where
    go vs [] _ = vs
    go vs (_:ps) [] = go (0:vs) ps []
    go vs (p:ps) xxs@((x,k):xs)
        | p == x    = go (k:vs) ps xs
        | otherwise = go (0:vs) ps xxs

favourites :: [Int] -> (Int,[Integer])
favourites xs = (m, map g $ filter f $ zip [0..] xs)
  where m = maximum xs
        f = (m==) . snd
        g = (primes!!) . fst

listV (V xs) = xs

query :: Array Int V -> (Int,Int) -> (Int,[Integer])
query a (i,j) = favourites $ listV (a!j - a!i)

out :: (Int,[Integer]) -> IO ()
out (m,ps) = putStrLn $ unwords $ show m : map show ps

readI :: Integral a => Text -> a
readI = fst . g . decimal where g (Right x) = x

pair [a,b] = (a,b)

main = withFile "numbers.txt" ReadMode $ \h -> do
        days <- accumulate . map f . T.lines <$> Tio.hGetContents h
        mapM_ (out . query days . parse) . g . T.lines =<< Tio.getContents
  where parse = pair . map readI . take 2 . T.words
        f = vectorize . factorize . readI
        g (x:xs) = take n xs where n = readI x

