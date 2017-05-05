import Data.List
import Data.Int

type I = Int64

triangle :: [I] -> Maybe I
triangle = perimeter . trips . sort
  where perimeter [] = Nothing
        perimeter xs = Just $ minimum $ map sum xs

trips xs = [[x,y,z] | x:ys <- tails xs, y:z:zs <- tails ys, x+y > z]

byLines f = interact $ unlines . f . lines

showCase (i,n) = concat ["Case #",show i,": ",p n]
  where p Nothing = "IMPOSSIBLE"
        p (Just n) = show n
showCases = map showCase . zip [1..]

main = byLines $ showCases . map (triangle . f . words) . g
  where g (l:ls) = take t ls where t = read l
        f (n:xs) = take (read n) $ map read xs
