{-# LANGUAGE OverloadedStrings #-}
import Network.Socket hiding (send,recv,sendTo,recvFrom)
import Network.Socket.ByteString (send,recv)
import qualified Data.ByteString.Char8 as B8
import Control.Applicative
import Control.Monad
import System.IO
import Data.Char
import Data.List
import Data.Ord

type State = [B8.ByteString]
type Client = Socket -> State -> IO ()

client :: State -> Client -> String -> IO ()
client state talk server = withSocketsDo $ do
    let (host,_:port) = break (':'==) server
    addr <- head <$> getAddrInfo Nothing (Just host) (Just port)
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock (addrAddress addr)
    talk sock state
    sClose sock

pattern = B8.filter (not . isSpace) . head . drop 2 . reverse

findStr s = not . B8.null . snd . B8.breakSubstring s
continue = findStr "continue" . last
gameOver = findStr "GAME OVER"
submit = findStr "Your submit key is:"
done msg = gameOver msg || submit msg

match pattern word = B8.length pattern == B8.length word
    && (and $ B8.zipWith f pattern word)
  where f p x = wildcard p || p == x
          where wildcard = not . isAlpha

select ws = fst . head . sortBy by . map count
  where count a = (a, length $ filter (B8.elem a) ws)
        by = flip $ comparing snd

hangman :: Client
hangman sock dictionary = start
  where
    start = next dictionary '*' ['A'..'Z']
    ask = B8.getLine >>= say
    say = send sock
    next ws x xs = do
      msg <- recv sock 1000
      unless (B8.null msg) $ do
        B8.putStr msg
        let text = B8.lines msg
        if continue text then ask >> start else
          unless (done msg) $ do
            let p = pattern text
                f = if B8.elem x p
                    then match p
                    else B8.notElem x
                ws' = filter f ws
                x' = select ws' xs
                guess = B8.singleton x'
            B8.putStrLn guess
            say guess
            next ws' x' $ xs \\ [x']

main = withFile "words.txt" ReadMode $ \h -> do
    contents <- B8.words <$> B8.hGetContents h
    client contents hangman "52.49.91.111:9988"
