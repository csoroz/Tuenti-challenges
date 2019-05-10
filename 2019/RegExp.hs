module RegExp (Rexp(..),enum) where
-- https://www.cs.utexas.edu/users/misra/Notes.dir/RegExp.pdf
-- Define concatenation and alternation to be associative.
-- concatenation has higher binding power than alternation.
infixr 5 :|
infixr 6 :.
data Rexp z = Phi      -- empty language
    | Nil              -- language containing null string
    | Single z         -- symbol from the alphabet
    | Rexp z :| Rexp z -- alternation
    | Rexp z :. Rexp z -- concatenation
    | Star (Rexp z)    -- Kleene closure

type Str z = [z] -- string of symbols

-- The result of function enum is an ordered list of strings of a regular expression.

enum :: Ord z => Rexp z -> [Str z]
enum re = case re of
    Phi      -> []     -- empty language
    Nil      -> [[]]   -- language containing null string only
    Single x -> [[x]]  -- convert x to string
    x :| y   -> enum x +++ enum y -- merge alternatives
    x :. y   -> enum x *** enum y -- concatenate
    Star x   -> closure (enum x)

-- The following functions —merge(+++), prod(***), and closure— are as given before.

(+++), (***) :: Ord z => [Str z] -> [Str z] -> [Str z]

[] +++ ys = ys
xs +++ [] = xs
xs@(x:xt) +++ ys@(y:yt) = case compare (metric x) (metric y) of
    LT -> x : (xt +++ ys)
    EQ -> x : (xt +++ yt)
    GT -> y : (xs +++ yt)

[] *** _  = []
_  *** [] = []
xs@(x:xt) *** ys@(y:yt) = (x++y) : (map (x++) yt +++ (xt *** ys))

closure :: Ord z => [Str z] -> [Str z]
closure []      = [[]]
closure ([]:xt) = closure xt
closure xs      = [] : (xs *** closure xs)

metric x = (length x, x)
