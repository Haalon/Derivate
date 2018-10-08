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

priority2 (Op "+" _) = 1
priority2 (Op "*" _) = 2
priority2 (Pow _ _) = 3
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

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)

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

data ExpTree = Num Double | Var String | Pow ExpTree ExpTree | Fun String ExpTree | Op String [ExpTree]
	deriving (Eq,Ord)

zero = Num 0
one  = Num 1
negOne = Num $ -1

infixl 6 +.+,-.-
infixl 7 *.*,/./
infixr 8 ^.^

(+.+) :: ExpTree -> ExpTree -> ExpTree
(+.+) (Op "+" ls) (Op "+" rs) = Op "+" $ merge ls rs
(+.+) (Op "+" ls) r = Op "+" $ insert r ls
(+.+) l (Op "+" rs) = Op "+" $ insert l rs
(+.+) l r = Op "+" $ insert l [r]

(*.*) :: ExpTree -> ExpTree -> ExpTree
(*.*) (Op "*" ls) (Op "*" rs) = Op "*" $ merge ls rs
(*.*) (Op "*" ls) r = Op "*" $ insert r ls
(*.*) l (Op "*" rs) = Op "*" $ insert l rs
(*.*) l r = Op "*" $ insert l [r]

(^.^) = Pow


(-.-) l r = l +.+ negOne *.* r
(/./) l r = l *.* r ^.^ negOne

neg :: ExpTree -> ExpTree
neg = (*.* negOne)

inv :: ExpTree -> ExpTree
inv = \l -> Pow l negOne

isConst :: ExpTree -> Bool
isConst (Num _)  = True
isConst         _  = False

noncommutative :: String -> Bool
noncommutative op = op /= "+" && op /= "*"

leftassoc :: String -> Bool
leftassoc op = op /= "^"


instance Show ExpTree where
	show (Num const) = if const >= 0 then show const else "(" ++ show const ++ ")"
	show (Var var) = var
	show (Fun name m) = name ++ "(" ++ show m ++ ")"
	show op@(Op s es) = intercalate s $ map show' es
		where
			show' e = if priority2 op > priority2 e then "(" ++ show e ++ ")" else show e
	show op@(Pow base pow) = b ++"^" ++ p
		where
			b = if priority2 op >= priority2 base then "(" ++ show base ++ ")" else show base
			p = if priority2 op > priority2 pow then "(" ++ show pow ++ ")" else show pow

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
		then (Const $ -1) : (polishInverse' ('*' :rest) False stack) --takes care of unary minus
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
		newstack = subtree : (tail $ tail stack)
		subtree = op (stack!!1) (head stack)
		op = case name of 
				"+" -> (+.+)
				"-" -> (-.-)
				"*" -> (*.*)
				"/" -> (/./)
				"^" -> (^.^)
			

mapSeparate :: (a->a)->[a]->[[a]]
mapSeparate f ls = mapSeparate' [] ls
	where
		mapSeparate' _ [] = []
		mapSeparate' top btm@(x:xs) = (top ++ [f x] ++ xs) : mapSeparate' (top++[x]) xs


derivateTree :: String -> ExpTree -> ExpTree
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
derivateTree var (Pow l r) = r *.* l ^.^ (r -.- Num 1) *.* dl +.+ (ln l)*.* l ^.^ r  *.* dr
	where
		dl = derivateTree var l
		dr = derivateTree var r
		ln = Fun "log"




simplifyConst :: ExpTree -> ExpTree
simplifyConst (Op "*" es) | elem zero es = zero
simplifyConst (Pow l r) | l == zero  = zero
simplifyConst (Pow l r) | r == zero  = one
simplifyConst (Pow l r) | l == one   = one
simplifyConst (Pow l r) | r == one   = l
simplifyConst a                         = a	


simplify (Op op es) = simplifyConst $ Op op $ map simplifyConst es
simplify (Fun f m)  = Fun f (simplify m) 
simplify a           = a

derivate :: String ->  String -> String
derivate var expr = show $ simplify $ derivateTree var $ createTree (polishInverse expr) []

derivate' :: String ->  String -> String
derivate' var expr = show  $ derivateTree var $ createTree (polishInverse expr) []

smp expr = simplify $ createTree (polishInverse expr) []
