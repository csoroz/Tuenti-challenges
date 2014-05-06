import Control.Applicative
import Prelude hiding (getContents)
import Data.Text.IO (getContents)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Read
import Data.Either
import Data.Maybe
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

readI = fst . g . decimal where g (Right x) = x

parse :: Text -> [[Int]]
parse = map (g 2) . T.lines
  where g n = map readI . take n . T.words

unique = map head . group . sort

process :: [[Int]] -> (Int, [Int], [[Int]])
process xs = (length zs, zs, map (map g) xs)
  where
    ys = concat xs
    zs = unique ys
    n = length zs
    m = M.fromList $ zip zs [1..]
    g = fromJust . flip M.lookup m

main = do
    (n,xs,calls) <- process . parse <$> getContents
    print n
    mapM_ print xs 
    mapM_ (putStrLn . unwords . map show) calls 
