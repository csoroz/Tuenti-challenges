import Data.Array.IO
import Data.List
import Data.Char

type Ground = IOUArray (Int,Int) Char

circuit :: String -> Ground -> IO ((Int,Int,Int,Int),Ground)
circuit xs a = loop (0,0) (1,0) (1,1,0,0) xs
  where
    loop _ _ ls [] = return (ls,a)
    loop (x,y) d@(dx,dy) ls (z:zz) = do
        let p = (x+dx,y+dy)
        writeArray a p (road d z)
        loop p (turn d z) (limits p ls) zz
    turn (v,0) '/'  = (0,v)
    turn (0,v) '/'  = (v,0)
    turn (0,v) '\\' = (-v,0)
    turn (v,0) '\\' = (0,-v)
    turn d _ = d
    road (0,_) '-' = '|'
    road  _     z  =  z
    limits (x,y) (xm,xn,ym,yn) = (min x xm, max x xn, min y ym, max y yn)

showCircuit :: ((Int,Int,Int,Int),Ground) -> IO [String]
showCircuit ((l,r,b,u),a) = return . reverse =<< mapM (row l r) [b..u]
  where
    row l r y = mapM (get y) [l..r] 
    get y x = readArray a (x,y)

solve :: String -> IO [String]
solve xs = showCircuit =<< circuit xs =<< newArray ((1-n,-n),(1+n,n)) ' '
  where n = length xs `div` 2 + 1

main =  mapM_ putStrLn =<< solve . head . lines =<< getContents
