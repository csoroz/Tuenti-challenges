
solve :: [Double] -> Double
solve = sqrt . sum . map (^2)

out z = reverse $ g $ reverse $ show $ (100*d + e) / 100
  where g = dropWhile (\x -> x=='0' || x=='.')
        i = round (z*100)
        (n,m) = divMod i 100
        (d,e) = (fromInteger n, fromInteger m)

parse (x:xs) = map (map read . take 2 . words) (take n xs)
  where n = read x

main = mapM_ (putStrLn . out . solve) . parse . lines =<< getContents
