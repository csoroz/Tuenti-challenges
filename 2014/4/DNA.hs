import Data.List
import Data.Maybe

connect valid a b xs = map (a:) (go a (b:xs))
    where go a xs
            | a == b    = [[]]
            | otherwise = [x:path | x <- xs, valid a x, path <- go x (xs \\ [x])]

validDNA [] _ = False
validDNA _ [] = False
validDNA (x:xs) (y:ys) 
    | x == y = validDNA xs ys
    | otherwise = xs == ys

solve (a:b:xs) = fromMaybe [] $ listToMaybe $ connect validDNA a b xs

showPath = intercalate "->"

main = putStrLn . showPath . solve . lines =<< getContents
