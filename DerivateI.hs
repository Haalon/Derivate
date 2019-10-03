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
import Data.List hiding(insert)

priority2 (Op "+" _ _) = 1
priority2 (Op "-" _ _) = 1
priority2 (Op "*" _ _) = 2
priority2 (Op "/" _ _) = 2
priority2 (Op "^" _ _) = 3
priority2 (Fun _ _) = 4
priority2 _ = 5

operators = ["+","-","*","/","^"]
priority (BinOp "+") = 1
priority (BinOp "-") = 1
priority (BinOp "*") = 2
priority (BinOp "/") = 2
priority (BinOp "^") = 3
priority (UnOp _) = 4
priority _ = -1
func = ["sin","cos","log","exp","tan","sqrt","acos", "asin", "atan"]

--data Operation = Sum | Sub | Mul | Div | Pow 

-- for RPN
data Token = Const Double | Name String | BinOp String | UnOp String | OpBr | ClBr | End
	deriving(Eq)

instance Show Token where
	show (Const d) = "Const " ++ show d
	show (Name s)   = "Name " ++ s 
	show (BinOp s) = "BinOp " ++ s
	show (UnOp s) = "UnOp " ++ s 
	show OpBr = "("
	show ClBr = ")"
	show End = ""

isOpBr OpBr = True
isOpBr _ = False

data ExpTree = Num Double | Var String | Fun String ExpTree | Op String ExpTree ExpTree
	deriving (Eq)

instance Num ExpTree where
  (+) = Op "+"
  (-) = Op "-"
  (*) = Op "*"
  negate = Op "*" (-1)
  abs = Fun "abs"
  signum = Fun "sgn"
  fromInteger = Num . fromInteger

instance Fractional ExpTree where
  fromRational = Num . fromRational
  recip = Op "/" 1
  (/) = Op "/"

instance Floating ExpTree where
  pi = Num pi
  (**) = Op "^"
  exp = Fun "exp"
  sqrt = Fun "sqrt"
  log = Fun "log"
  sin = Fun "sin"
  cos = Fun "cos"
  sinh = Fun "sinh"
  cosh = Fun "cosh"
  asin = Fun "asin"
  acos = Fun "acos"
  atan = Fun "atan"
  asinh = Fun "asinh"
  acosh = Fun "acosh"
  atanh = Fun "atanh"



isConst :: ExpTree -> Bool
isConst (Num _)  = True
isConst (Op _ l r) = (isConst l) && (isConst r)
isConst (Fun _ m) = (isConst m)
isConst         _  = False

noncommutative :: String -> Bool
noncommutative op = op /= "+" && op /= "*"

leftassoc :: String -> Bool
leftassoc op = op /= "^"


instance Show ExpTree where
	show (Num const) = if const >= 0 then show const else "(" ++ show const ++ ")"
	show (Var var) = var
	show (Fun name m) = name ++ "(" ++ show m ++ ")"
	show op@(Op s l r) = ls ++ s ++ rs
		where
			ls = if priority2 op > priority2 l || lCase  then "(" ++ show l ++ ")" else show l
			rs = if priority2 op > priority2 r || rCase then "(" ++ show r ++ ")" else show r
			lCase = noncommutative s && priority2 op == priority2 l && (not $ leftassoc s)
			rCase = noncommutative s && priority2 op == priority2 r &&       leftassoc s

parseName :: String -> (Token, String)
parseName ls = (tok, rest)
	where
		(pre, rest) = span isAlpha ls
		tok = if elem pre func then UnOp pre else Name pre

parseNumb ls = (Const $ read pre, rest)
	where
		(pre, rest) = span (\x -> isDigit x || x =='.') ls

parseDelm ls = (BinOp delim, fromJust $ stripPrefix delim ls)
	where
		eqList = map and $ map (zipWith (==) ls) operators
		delim = (!!) operators $ fromJust $ elemIndex True eqList		

parseToken :: String -> (Token, String)
parseToken [] = (End, [])
parseToken (' ':ls) = parseToken ls
parseToken ('(':ls) = (OpBr, ls)
parseToken (')':ls) = (ClBr, ls)
parseToken expr@(l:ls) 
	| isAlpha l = parseName expr
	| isDigit l = parseNumb expr
	| otherwise = parseDelm expr

polishInverse :: String -> [Token]
polishInverse expr = polishInverse' expr True []

polishInverse' :: String -> Bool -> [Token] -> [Token]
polishInverse' expr flag stack = case parseToken expr of 
	(tok@(Name _), rest) -> tok: (polishInverse' rest False stack)
	(tok@(Const _), rest) ->tok: (polishInverse' rest False stack)
	(tok@(UnOp _), rest) ->polishInverse' rest False $ tok:stack
	(tok@(OpBr), rest) ->polishInverse' rest True $ tok:stack
	(tok@(ClBr), rest) ->add ++ (polishInverse' rest False (tail left))
		where
			(add,left) = span (not.isOpBr) stack	
	(tok@(BinOp op), rest) -> if op == "-" && flag
		then (Const $ -1) : (polishInverse' ('*' :rest) False stack)
		else add ++ (polishInverse' rest False (tok:left))
			where
				(add,left) = span predicate stack
				predicate x = priority tok < priority x || (op /= "^" && priority tok <= priority x)
	(tok@(End), _) -> stack++[tok]

createTree :: [Token] -> [ExpTree] -> ExpTree
createTree ((End):_) stack   = head stack
createTree ((Name name):ls) stack = createTree ls $ (Var name):stack
createTree ((Const val):ls) stack = createTree ls $ (Num val):stack
createTree ((UnOp name):ls) stack = createTree ls newstack
	where
		subtree = Fun name $ head stack
		newstack = subtree : (tail stack)
createTree ((BinOp name):ls) stack = createTree ls newstack
	where
		subtree = Op name (stack!!1) (head stack)
		newstack = subtree : (tail $ tail stack)

derivateTree :: String -> ExpTree -> ExpTree
derivateTree var (Num _) = 0
derivateTree var (Var name) 
	| name == var = 1
	| otherwise   = 0
derivateTree var f@(Fun name m)
	| name == "sin"  = cos m * dm
	| name == "cos"  = negate (sin m) * dm
	| name == "log"  = dm / m
	| name == "exp"  = f * dm
	| name == "tan"  = dm / (cos m) ** 2
	| name == "sqrt" = 0.5 * dm / f
	| name == "asin" = dm / (1 - m ** 2) ** 0.5
	| name == "acos" = negate dm / (1 - m ** 2) ** 0.5
	| name == "atan" = dm / (1 + m ** 2)
	where
		dm = derivateTree var m
derivateTree var (Op name l r)
	| name == "+" = dl + dr
	| name == "-" = dl - dr
	| name == "*" = dl * r + l * dr
	| name == "/" = (dl * r - l * dr) / (r ** 2)
	| name == "^" = r * l ** (r - 1) * dl + (log l)* l ** r  * dr
	where
		dl = derivateTree var l
		dr = derivateTree var r


simplifyConst :: ExpTree -> ExpTree
simplifyConst (Op "*" l r) | l == 0  = 0
simplifyConst (Op "*" l r) | r == 0  = 0
simplifyConst (Op "*" l r) | l == 1   = r
simplifyConst (Op "*" l r) | r == 1   = l
simplifyConst (Op "+" l r) | l == 0  = r
simplifyConst (Op "+" l r) | r == 0  = l
simplifyConst (Op "-" l r) | l == 0  = negate r
simplifyConst (Op "-" l r) | r == 0  = l
simplifyConst (Op "/" l r) | l == 0  = 0
simplifyConst (Op "/" l r) | r == 1   = l 
simplifyConst (Op "^" l r) | l == 0  = 0
simplifyConst (Op "^" l r) | r == 0  = 1
simplifyConst (Op "^" l r) | l == 1   = 1
simplifyConst (Op "^" l r) | r == 1   = l
simplifyConst a            | isConst a  = calc a
simplifyConst a                         = a	


calc :: ExpTree -> ExpTree
calc (Op op (Num lv) (Num rv)) = case op of 
	"+" -> Num (lv+rv)
	"-" -> Num (lv-rv)
	"*" -> Num (lv*rv)
	"/" -> Num (lv/rv)
	"^" -> Num (lv**rv)
calc (Fun f (Num mv)) = case f of 
	"sin" -> Num $ sin mv
	"cos" -> Num $ cos mv
	"log" -> Num $ log mv
	"exp" -> Num $ exp mv
calc a = a

simplify (Op op l r) = simplifyConst $ Op op (simplify l) (simplify r)
simplify (Fun f m)  = Fun f (simplify m) 
simplify a           = a

derivate :: String ->  String -> String
derivate var expr = show  $ simplify $ derivateTree var $ createTree (polishInverse expr) []

derivate' :: String ->  String -> String
derivate' var expr = show  $ derivateTree var $ createTree (polishInverse expr) []

smp expr = simplify $ createTree (polishInverse expr) []


main :: IO ()
main = do
  putStrLn "For the Great Good!";
  a <- getArgs; 
  print $ head a
