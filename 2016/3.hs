{-# LANGUAGE FlexibleInstances #-}
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Applicative ((<$>))
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

type Value = Char
type Label = String
data TO = L | R deriving Show
data Ins a = Write a | Move TO | Goto Label deriving Show
type Instr = Ins Value
type Vals = Map Value [Instr]
type Code = Map Label Vals
type Tape = Tap Value
data Tap a = T a [a] [a]

instance Show (Tap Char) where
  show (T x ls rs) = concat [reverse $ tr ls,[x],tr rs]
    where tr = takeWhile (/=' ')

look :: Ord a => a -> Map a b -> b
look a = fromJust . M.lookup a

run :: Code -> Tape -> Tape
run code = goto "start"
  where
    goto "end" = id
    goto s = exec (look s code)
    exec m = fetch
      where
        fetch tape = execute (f tape) tape
          where f (T x _ _) = look x m
        execute [] = fetch
        execute (Goto s:_) = goto s
        execute (y:ys) = execute ys . exe y 
          where
            exe (Write x) (T _ ls rs)     = T x ls rs
            exe (Move  L) (T x (l:ls) rs) = T l ls (x:rs)
            exe (Move  R) (T x ls (r:rs)) = T r (x:ls) rs

lexer = Token.makeTokenParser emptyDef
identifier = Token.identifier  lexer
reserved   = Token.reserved    lexer
symbol     = Token.symbol      lexer
colon      = Token.colon       lexer
literal    = Token.charLiteral lexer
integer    = Token.decimal     lexer

prog = do
    symbol "---"
    label "code"
    cs <- code
    ts <- tapes
    eof
    return (cs,ts)
  where
    after p q = do a <- p; q; return a
    lab p = after p colon
    label s = lab (reserved s) >> return ()
    state = Goto  <$> (label "state" >> identifier)
    write = Write <$> (label "write" >> literal)
    move  = Move  <$> (label "move"  >> left_right)
      where left_right = (reserved "left"  >> return L)
                     <|> (reserved "right" >> return R)
    instr = write <|> move <|> state
    sep p q g = do v <- lab p; vs <- q; return (v, g vs)
    val   = sep literal (many1 instr) id
    vals  = sep identifier (many1 val) M.fromList
    tape  = sep integer (after quo newline) id
    tapes = manyTill tape (symbol "...")
    code  = M.fromList <$> (manyTill vals $ label "tapes")
    quo = between q q (many1 $ noneOf "'")
            where q = char '\''

parseCode :: String -> (Code,[(Integer,String)])
parseCode = g . parse prog "" where g (Right a) = a

trim = reverse . sp . reverse . sp
  where sp = dropWhile isSpace

fromList (x:xs) = T x spaces (xs++spaces)
  where spaces = repeat ' '

printCase (i,t) = putStrLn $ concat $ ["Tape #",show i,": ",show t]

main = do
    (code,tapes) <- parseCode . unlines . map trim . lines <$> getContents
    let (is,ts) = unzip tapes
    mapM_ printCase $ zip is $ map (run code . fromList) ts
