import Codec.Binary.Base64.String -- cabal install base64-string
import Data.Tuple
import Data.List
import Data.Char

chunks _ [] = []
chunks k xs = x : chunks k xx where (x,xx) = splitAt k xs

digits b = reverse . unfoldr f
  where f 0 = Nothing
        f x = Just $ swap $ divMod x b

undigits b = foldl' (\m n -> b*m + n) 0

type Bits = [Int]

decodeB64 :: String -> Bits
decodeB64 = map fromInteger . to . map (toInteger . ord) . decode
  where to = tail . digits 2 . undigits 256 . (1:)

fromBits :: Bits -> Integer
fromBits = undigits 2 . map toInteger

data Endian = L | B deriving (Read,Show)

endian :: Endian -> Bits -> Bits
endian L = concat . reverse . chunks 8
endian B = id

parse :: String -> (Int,Endian,[a]->[a])
parse = g . break (not . isDigit)
  where g (xs,e:r) = (read xs, read [e], rev r)
        rev ['R'] = reverse
        rev   _   = id

pieces :: (Bits,[String]) -> [Integer]
pieces (_,[]) = []
pieces (bits,d:ds) = fromBits (r $ endian e xs) : pieces (ys,ds)
  where (xs,ys) = splitAt n bits
        (n,e,r) = parse d

main = mapM_ print . pieces . g . lines =<< getContents
  where g (x:y:zs) = (decodeB64 x, take n zs)
          where n = read y
