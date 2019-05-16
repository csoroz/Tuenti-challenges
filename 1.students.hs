import Text.Printf
import System.IO
import Control.Applicative
import Data.List.Split
import Data.List

type Name = String
type Value = [String]
type Students = [(Name,Value)]

csv = wordsBy (==',')
uncsv = intercalate ","

loadStudents :: Handle -> IO Students
loadStudents h = return . map (g . csv) . lines =<< hGetContents h
  where g (x:xs) = (x,xs)

parse (x:xs) = map csv $ take n xs
  where n = read x

solve :: Value -> Students -> [Name]
solve x = sort . fst . unzip . filter ((x==).snd)

printCase :: (Int,[String]) -> IO ()
printCase (i,ys) = printf "Case #%d: %s\n" i (g ys)
  where g [] = "NONE"
        g ys = uncsv ys

main = do xs <- parse . lines <$> getContents
          withFile "students" ReadMode $ \h -> do
            db <- loadStudents h
            mapM_ printCase $ zip [1..] (map (flip solve db) xs)
