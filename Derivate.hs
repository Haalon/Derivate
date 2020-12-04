{-|
Module      : Derivate
Description : operations with vectors combined with Stdgens (GVec-s)
Copyright   : Just Nothing
Stability   : in progress
-}
module Derivate where
import System.Environment
import Data.Char
import Data.Maybe
import Data.List
import Parser
import Control.Applicative ((<|>))

priority (Op "+" _) = 1
priority (Op "*" _) = 2
priority (Pow _ _) = 3
priority (Fun _ _) = 4
priority _ = 5

func = ["sin","cos","log","exp","tan","sqrt","acos", "asin", "atan"]

-- abstract helper functions

type Cmp a = a->a->Ordering
type Fuse a = a->a->a

mapSeparate :: (a->a)->[a]->[[a]]
mapSeparate f ls = mapSeparate' [] ls
    where
        mapSeparate' _ [] = []
        mapSeparate' top btm@(x:xs) = (top ++ [f x] ++ xs) : mapSeparate' (top++[x]) xs

data Exp = Num Double | Var String | Op String [Exp] | Pow Exp Exp | Fun String Exp 
    deriving (Eq,Ord)

isNum :: Exp -> Bool
isNum (Num _)  = True
isNum       _  = False

rmNum :: [Exp] -> [Exp]
rmNum = dropWhile isNum

getNum _ (val@(Num _):_) = val
getNum val _ = val

instance Show Exp where
    show (Var var) = var
    show (Fun name m) = name ++ "(" ++ show m ++ ")"
    show ex@(Op s (l:ls)) = showOp "" l ++ concatMap (showOp s) ls
        where
            getVal (Num val) = val
            getVal (Op _ ((Num val):_)) = val
            getVal (Pow _ pow) = getVal pow
            getVal _ = 1
            showOp op elem  | priority ex > priority elem = op ++ "(" ++ show elem ++ ")"
            showOp "+" elem | getVal elem < 0 = "-" ++ (show $ neg elem)
            showOp "*" elem | getVal elem < 0 = "/" ++ (show $ inv elem)
            -- showOp "" elem  | s == "*" && getVal elem < 0 = "1/" ++ (show $ inv elem)
            -- showOp _ elem@(Op "*" ((Num val) : _)) | val < 0 = "-" ++ (show $ neg elem)
            showOp op elem = op ++ show elem
            -- showHead (Op _ ((Num val):rest)) | val < 0 = show val ++ concatMap show rest
            -- showHead other = show other  
            -- showTail exp@(Pow b (Num n)) "*" | n < 0      = "/" ++ handlePriority ex (inv exp) (>=) -- replace a*b^(-c) with a/b^c
            -- showTail exp op = op ++ handlePriority ex exp (>)
            -- handlePriority parent child cmp = if priority parent `cmp` priority child then "(" ++ show child ++ ")" else show child
    show op@(Pow base pow) = b ++ "^" ++ p
        where
            b = if priority op >= priority base then "(" ++ show base ++ ")" else show base
            p = if priority op > priority pow then "(" ++ show pow ++ ")" else show pow
    show (Num const) = if const >= 0 then showC const else "(" ++ showC const ++ ")"
        where 
            showC n = if f == 0 then show r else show n
                where (r,f) = properFraction n

zero = Num 0
one  = Num 1
negOne = Num $ -1

infixl 6 +.+,-.-
infixl 7 *.*,/./
infixr 8 ^.^
infix 4 =*=

-- Modified Constructor. it will not create trivial Op
-- Sometimes there may be leftover zeroes or other usless consts
op' :: String -> [Exp] -> Exp
op' _ [n] = n
op' "*" (l:_) | l == zero = zero
op' "*" es = 
    let res = filter (\e -> getNum zero [e] /= one) es
    in case res of
        [] -> one
        _  -> Op "*" res
op' "+" es = 
    let res = filter (\e -> getNum one [e] /= zero) es
    in case res of
        [] -> zero
        _  -> Op "+" res
op' op a  = Op op a

-- these functions can create neutral Nums (zero for 0 or 1 for *)
-- they are removed in op'
insertExp :: Cmp Exp -> Fuse Exp -> Exp -> [Exp] -> [Exp]
insertExp _ _ e [] = [e]
insertExp cmp fuse x ys@(y:ys') = case cmp x y of
    GT -> y : insertExp cmp fuse x ys'
    EQ -> (fuse x y) : ys'
    _  -> x : ys

mergeExp :: Cmp Exp -> Fuse Exp -> [Exp] -> [Exp] -> [Exp]
mergeExp _ _ [] x = x
mergeExp _ _ x [] = x
mergeExp cmp fuse (x:xs) (y:ys) = case cmp x y of
    GT -> y : mergeExp cmp fuse (x:xs) ys
    EQ -> fuse x y : mergeExp cmp fuse xs ys
    _  -> x : mergeExp cmp fuse xs (y:ys)



--likeness of two Terms for products. i.e can we combine their powers?
(=*=) :: Cmp Exp
(=*=) (Num _) (Num _) = EQ
(=*=) (Pow bl pl) (Pow br pr) = compare bl br
(=*=) (Pow bl _) r = compare bl r
(=*=) l (Pow br _) = compare l br
(=*=) l r = compare l r

-- How do we combine like terms for multiplication
(*=*) :: Fuse Exp
(*=*) (Num ln) (Num rn) = Num $ ln*rn
(*=*) (Pow bl pl) (Pow _ pr) = bl ^.^ (pl +.+ pr)
(*=*) (Pow bl pl) r = r ^.^ (pl +.+ one)
(*=*) l (Pow br pr) = l ^.^ (pr +.+ one)
(*=*) l r =  l ^.^ (Num 2) -- we assume here that l =*= r

(*.*) :: Exp -> Exp -> Exp
(*.*) l r | l==one  = r
(*.*) l r | r==one  = l
(*.*) l r | l==zero = zero
(*.*) l r | r==zero = zero

(*.*) (Op "*" ls) (Op "*" rs) = op' "*" $ mergeExp (=*=) (*=*) ls rs
(*.*) (Op "*" ls) r = op' "*" $ insertExp (=*=) (*=*) r ls
(*.*) l (Op "*" rs) = op' "*" $ insertExp (=*=) (*=*) l rs
(*.*) l r = op' "*" $ insertExp (=*=) (*=*) r [l]


--likeness of two Terms for sums. i.e can we combine their coefficients?
(=+=) :: Cmp Exp
(=+=) (Num _) (Num _) = EQ
(=+=) (Op "*" ls) (Op "*" rs) = compare (rmNum ls) (rmNum rs)
(=+=) (Op "*" ls) r = compare (rmNum ls) [r]
(=+=) l (Op "*" rs) = compare [l] (rmNum rs)
(=+=) l r = compare l r

-- How do we combine like terms for addition
(+=+) :: Fuse Exp
(+=+) (Num ln) (Num rn) = Num $ ln+rn
(+=+) (Op "*" ls) (Op "*" rs) = (getNum one ls +.+ getNum one rs) *.* op' "*" (rmNum ls)
(+=+) (Op "*" [val,_]) r =  (val +.+ one) *.* r -- it's not obvious but in this case first elem
(+=+) l (Op "*" [val,_]) =  (val +.+ one) *.* l -- will always contatin 'Num' val
(+=+) l r = (Num 2) *.* l -- we assume here that l == r

(+.+) :: Exp -> Exp -> Exp
(+.+) l r | l==zero = r
(+.+) l r | r==zero = l

(+.+) (Op "+" ls) (Op "+" rs) = op' "+" $ mergeExp (=+=) (+=+) ls rs
(+.+) (Op "+" ls) r = op' "+" $ insertExp (=+=) (+=+) r ls
(+.+) l (Op "+" rs) = op' "+" $ insertExp (=+=) (+=+) l rs
(+.+) l r = op' "+" $ insertExp (=+=) (+=+) r [l]


(^.^) (Num ln) (Num rn) = Num $ ln**rn
(^.^) l r | r==one = l
(^.^) l r | r==zero = one
(^.^) l r | l==zero = zero
(^.^) l r | l==one = one
(^.^) (Pow bl pl) r = bl ^.^ (pl *.* r) -- (a^b)^c == a^(b*c)
(^.^) l r = Pow l r


(-.-) l r = l +.+ negOne *.* r
(/./) l r = l *.* r ^.^ negOne

neg :: Exp -> Exp
neg =  (*.* negOne)

inv :: Exp -> Exp
inv = \l -> l ^.^ negOne


topP :: Parser0 Exp
topP = do
    spaces *> termsP <* spaces

termsP :: Parser0 Exp
termsP = do
    let combine = (reserved "+" *> return (+.+)) <|> (reserved "-" *> return (-.-))
    chainl1 multsP combine

multsP :: Parser0 Exp
multsP = do
    let combine = (reserved "*" *> return (*.*)) <|> (reserved "/" *> return (/./))
    chainl1 powsP combine

powsP :: Parser0 Exp
powsP = do
    let combine = reserved "^" *> return (^.^)
    chainr1 finalP combine

funcP :: String -> Parser0 Exp
funcP name = do
    n <- reserved name
    inner <- parens "(" ")" topP
    return $ Fun n inner

numP :: Parser0 Exp
numP = Num <$> floating

uminP :: Parser0 Exp
uminP = do
    reserved "-"
    exp <- finalP
    return $ negOne *.* exp

varP :: Parser0 Exp
varP = Var <$> alphaNum

finalP :: Parser0 Exp
finalP = (choice $ funcP <$> func) 
    <|> (parens "(" ")" topP)
    <|> numP
    <|> uminP
    <|> varP

            
derivateTree :: String -> Exp -> Exp
derivateTree var (Num _) = Num 0
derivateTree var (Var name) 
    | name == var = Num 1
    | otherwise   = Num 0
derivateTree var f@(Fun name m)
    | name == "sin"  = Fun "cos" m *.* dm
    | name == "cos"  = neg (Fun "sin" m) *.* dm
    | name == "log"  = dm /./ m
    | name == "exp"  = f *.* dm
    | name == "tan"  = dm /./ (Fun "cos" m) ^.^ (Num 2)
    | name == "sqrt" = Num 0.5 *.* dm /./ f
    | name == "asin" = dm /./ (one -.- m ^.^ (Num 2)) ^.^ (Num 0.5)
    | name == "acos" = neg dm /./ (one -.- m ^.^ (Num 2)) ^.^ (Num 0.5)
    | name == "atan" = dm /./ (one +.+ m ^.^ (Num 2))
    where
        dm = derivateTree var m
derivateTree var (Op name es)
    | name == "+" = foldl1' (+.+) de1
    | name == "*" = foldl1' (+.+) de2
    where
        de1 = map (derivateTree var) es
        de2 = map (foldl1' (*.*)) $ mapSeparate (derivateTree var) es
derivateTree var (Pow l r) = r *.* l ^.^ (r -.- Num 1) *.* dl +.+ (ln l) *.* l ^.^ r  *.* dr
    where
        dl = derivateTree var l
        dr = derivateTree var r
        ln = Fun "log"


smp = runParser0 topP

derivate :: String ->  String -> String
derivate var sexp = show $ derivateTree var $ smp sexp



