import Text.Printf
import Data.List
import Data.Char
import Data.Text.Read
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
-- cabal install charset
import qualified Data.CharSet as CS
import Data.CharSet.Unicode.Category

readI :: Integral a => Text -> a
readI = fst . g . decimal where g (Right x) = x

isNd x = CS.member x decimalNumber

toDigit x | isDigit x = x
          | isNd x = chr (ord '0' + dg)
          | otherwise = x
  where dg = if any p [(0x966,0xDEF),(0x1946,0x194F)
                      ,(0x11066,0x1106F),(0x11136,0x1113F)]
                then dx 6
                else let a = 0x1D7CE in
                      if p (a,0x1D7FF) 
                      then mod (n - a) 10
                      else dx 0
            where dx a = mod (n - a) 0x10
                  p (a,b) = a<=n && n<=b 
                  n = ord x

clean :: Text -> Maybe Integer
clean x = if T.all isDigit y then Just (readI y) else Nothing
  where y = T.map toDigit $ T.strip x

showCase (i,n) = concat ["Case #",show i,": ", p n]
  where p (Just n) = printf "%x" n
        p Nothing = "N/A"

showCases = mapM_ (putStrLn . showCase) . zip [1..]

main = showCases . map clean . g . T.lines . T.decodeUtf16LE =<< B.getContents
  where g (l:ls) = take c ls where c = readI $ T.dropWhile (not.isDigit) l
