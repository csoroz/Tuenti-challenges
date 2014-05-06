import Network.Socket
import System.IO
import Data.List
import Data.Char
import Data.Hex -- cabal install hex

echo sock = do 
    x <- recv sock 1000
    let n = length "CLIENT->SERVER:"
    let y = drop n x
    send sock y
    return y

main = do 
    pass <- getContents
    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "54.83.207.90"
    connect sock (SockAddrInet 6969 addr)
    xs <- sequence $ replicate 6 $ echo sock
    -- putStr $ drop 7 $ xs!!5
    -- unhex $ filter isHexDigit
    mapM_ putStrLn (pass:xs)

