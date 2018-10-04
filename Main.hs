{-|
Module      : Main
Description : Main module, running script.
Copyright   : Just Nothing
Stability   : Stable
-}
import System.Environment
import Data.Char
import Data.Maybe
import Data.List

--data Op = Sum | Mul | Pow | Div | Sub
--data Func = Sin | Cos | Log | Exp

priority2 (Op "+" _ _) = 1
priority2 (Op "-" _ _) = 1
priority2 (Op "*" _ _) = 2
priority2 (Op "/" _ _) = 2
priority2 (Op "^" _ _) = 3
priority2 (Func _ _) = 4
priority2 _ = 5

operators = ["+","-","*","/","^"]
priority (BinOp "+") = 1
priority (BinOp "-") = 1
priority (BinOp "*") = 2
priority (BinOp "/") = 2
priority (BinOp "^") = 3
priority (UnOp _) = 4
priority _ = -1
func = ["sin","cos","log","exp"]

-- for RPN
data Token = Const Double | Var String | BinOp String | UnOp String | OpBr | ClBr | End

{--
data Term = Term {
	t_coeff :: Double
	t_elems :: [Factor]
}

data Factor = Term {
	f_power :: Double
	f_elems :: [Factors]
}
--}

instance Show Token where
	show (Const d) = "Const " ++ show d
	show (Var s)   = "Var " ++ s 
	show (BinOp s) = "BinOp " ++ s
	show (UnOp s) = "UnOp " ++ s 
	show OpBr = "("
	show ClBr = ")"
	show End = ""

isOpBr OpBr = True
isOpBr _ = False

data ExpTree = LeafC Double | LeafV String | Func String ExpTree | Op String ExpTree ExpTree
	deriving (Eq)

infixl 6 +.+,-.-
infixl 7 *.*,/./
infixr 8 ^.^

(+.+) :: ExpTree -> ExpTree -> ExpTree
(+.+) = Op "+"
(-.-) = Op "-"
(*.*) = Op "*"
(/./) = Op "/"
(^.^) = Op "^"

zero = LeafC 0
one  = LeafC 1

neg :: ExpTree -> ExpTree
neg = Func "-"

inv :: ExpTree -> ExpTree
inv = Op "/" (LeafC 1)

isConst :: ExpTree -> Bool
isConst (LeafC _)  = True
isConst (Op _ l r) = (isConst l) && (isConst r)
isConst (Func _ m) = (isConst m)
isConst         _  = False



instance Show ExpTree where
	show (LeafC const) = show const
	show (LeafV var) = var
	show (Func name m) = name ++ "(" ++ show m ++ ")"
	show op@(Op s l r) = ls ++ s ++ rs
		where
			ls = if priority2 op > priority2 l || leftPow op  then "(" ++ show l ++ ")" else show l
			rs = if priority2 op > priority2 r then "(" ++ show r ++ ")" else show r
			leftPow (Op "^" (Op "^" _ _) _) = True -- Because of right assosiation
			leftPow _ = False
{-
prefixBelong :: [a]->[[a]]->Bool
prefixBelong list group = or $ map (\x -> x `isPrefixOf` list) group
-}

parseName :: String -> (Token, String)
parseName ls = (tok, rest)
	where
		(pre, rest) = span isAlpha ls
		tok = if elem pre func then UnOp pre else Var pre

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

polishInverse :: String -> [Token] -> [Token]
polishInverse expr stack = case parseToken expr of 
	(tok@(Var _), rest) -> tok: (polishInverse rest stack)
	(tok@(Const _), rest) ->tok: (polishInverse rest stack)
	(tok@(UnOp _), rest) ->polishInverse rest $ tok:stack
	(tok@(OpBr), rest) ->polishInverse rest $ tok:stack
	(tok@(ClBr), rest) ->add ++ (polishInverse rest (tail left))
		where
			(add,left) = span (not.isOpBr) stack	
	(tok@(BinOp _), rest) -> add ++ (polishInverse rest (tok:left))
		where
			(add,left) = span (\x -> priority tok < priority x) stack
	(tok@(End), _) -> stack++[tok]

createTree :: [Token] -> [ExpTree] -> ExpTree
createTree ((End):_) stack   = head stack
createTree ((Var name):ls) stack   = createTree ls $ (LeafV name):stack
createTree ((Const val):ls) stack  = createTree ls $ (LeafC val):stack
createTree ((UnOp name):ls) stack  = createTree ls newstack
	where
		subtree = Func name $ head stack
		newstack = subtree : (tail stack)
createTree ((BinOp "-"):ls) stack | length stack == 1 = createTree ls newstack -- uni minus
	where
		subtree = Func "-" $ head stack
		newstack = subtree : (tail stack)
createTree ((BinOp name):ls) stack = createTree ls newstack
	where
		subtree = Op name (stack!!1) (head stack)
		newstack = subtree : (tail $ tail stack)


derivateTree :: String -> ExpTree -> ExpTree
derivateTree var (LeafC _) = LeafC 0
derivateTree var (LeafV name) 
	| name == var = LeafC 1
	| otherwise   = LeafC 0
derivateTree var f@(Func name m)
	| name == "sin" = (Func "cos" m) *.* dm
	| name == "cos" = (neg (Func "sin" m)) *.* dm
	| name == "log" = dm /./ m
	| name == "exp" = f *.* dm
	where
		dm = derivateTree var m
derivateTree var (Op name l r)
	| name == "+" = dl +.+ dr
	| name == "-" = dl -.- dr
	| name == "*" = dl *.* r +.+ l *.* dr
	| name == "/" = (dl *.* r -.- l *.* dr) /./ (r ^.^ (LeafC 2))
	| name == "^" = r *.* l ^.^ (r -.- LeafC 1) *.* dl +.+ (ln l)*.* l ^.^ r  *.* dr
	where
		dl = derivateTree var l
		dr = derivateTree var r
		ln = Func "log" 


simplifyConst :: ExpTree -> ExpTree
simplifyConst (Op "*" l r) | l == zero  = zero
simplifyConst (Op "*" l r) | r == zero  = zero
simplifyConst (Op "*" l r) | l == one   = r
simplifyConst (Op "*" l r) | r == one   = l
simplifyConst (Op "+" l r) | l == zero  = r
simplifyConst (Op "+" l r) | r == zero  = l
simplifyConst (Op "-" l r) | l == zero  = r
simplifyConst (Op "-" l r) | r == zero  = l
simplifyConst (Op "/" l r) | l == zero  = zero
simplifyConst (Op "^" l r) | l == zero  = zero
simplifyConst (Op "^" l r) | r == zero  = one
simplifyConst (Op "^" l r) | l == one   = one
simplifyConst (Op "^" l r) | r == one   = l
simplifyConst a            | isConst a  = calc a
simplifyConst a                         = a	


calc :: ExpTree -> ExpTree
calc (Op op (LeafC lv) (LeafC rv)) = case op of 
	"+" -> LeafC (lv+rv)
	"-" -> LeafC (lv-rv)
	"*" -> LeafC (lv*rv)
	"/" -> LeafC (lv/rv)
	"^" -> LeafC (lv**rv)
calc (Func f (LeafC mv)) = case f of 
	"sin" -> LeafC $ sin mv
	"cos" -> LeafC $ cos mv
	"log" -> LeafC $ log mv
	"exp" -> LeafC $ exp mv
calc a = a

simplify (Op op l r) = simplifyConst $ Op op (simplify l) (simplify r)
simplify (Func f m)  = Func f (simplify m) 
simplify a           = a

derivate :: String ->  String -> String
derivate var expr = show  $ simplify $ derivateTree var $ createTree (polishInverse expr []) []

smp expr = simplify $ createTree (polishInverse expr []) []


main :: IO ()
main = do
  putStrLn "For the Great Good!";
  a <- getArgs; 
  print $ head a
