import Control.Applicative
import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read
import Data.List
import Data.Ord

query :: (Int,Int) -> [Text] -> [(Text,Int)]
query (a,b) = sortBy (flip $ comparing snd) . g . take n . drop i
  where i = a - 1
        n = b - i
        g = map count . group . sort
        count ys = (head ys, length ys)

printCase (i,ys) = putStrLn $ concat $ ["Case #",show i,": ",intercalate "," $ map g $ take 3 ys]
  where g (xs,n) = concat [T.unpack xs," ",show n]

readI :: Integral a => Text -> a
readI = fst . g . decimal where g (Right x) = x

main = withFile "corpus.txt" ReadMode $ \h -> do
    xs <- T.words <$> TIO.hGetContents h
    zs <- map (flip query xs . g) . f . T.lines <$> TIO.getContents
    mapM_ printCase $ zip [1..] zs
  where f (l:ls) = take n ls where n = readI l
        g = pair . map readI . take 2 . T.words
        pair [a,b] = (a,b)
