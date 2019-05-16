import Control.Applicative
import System.IO hiding (hGetContents)
import Data.Text.IO (hGetContents)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Read
import Data.Either
import Data.Maybe
import Data.List
import qualified Data.IntMap.Strict as M
import Data.Equivalence.Persistent -- cabal install persistent-equivalence

solve :: Int -> (Int, Int) -> [(Int,(Int,Int))] -> Maybe Int
solve n (a,b) = go $ emptyEquivalence (1,n)
  where
    go _ [] = Nothing
    go contacts ((i,(x,y)):zs)
      | equiv contacts' a b = Just i
      | otherwise = go contacts' zs
      where contacts' = equate x y contacts

---------------------------------------------------------------------------
readI = fst . g . decimal where g (Right x) = x

pair [a,b] = (a,b)

calls :: Text -> (Int, [Int], [(Int,Int)])
calls = go . T.lines
  where go (x:xs) = (n, map readI ys, map (pair . g 2) zs)
          where g k = map readI . take k . T.words
                (ys,zs) = splitAt n xs
                n = readI x

parse :: String -> [Int]
parse = map read . take 2 . lines

out = putStrLn . maybe "Not connected" (("Connected at "++) . show)

-- Preprocess calls with: Calls.exe < phone_call.log > calls.log
main = do
    ab <- parse <$> getContents
    withFile "calls.log" ReadMode $ \h -> do
        (n,ys,zs) <- calls <$> hGetContents h
        let m = M.fromList $ zip ys [1..]
        let ab' = pair $ map (fromJust . flip M.lookup m) ab
        out $ solve n ab' $ zip [0..] zs
