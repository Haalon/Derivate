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
func = ["sin","cos","log","exp"]

--data Operation = Sum | Sub | Mul | Div | Pow | 

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

infixl 6 +.+,-.-
infixl 7 *.*,/./
infixr 8 ^.^

(+.+) :: ExpTree -> ExpTree -> ExpTree
(+.+) = Op "+"
(-.-) = Op "-"
(*.*) = Op "*"
(/./) = Op "/"
(^.^) = Op "^"

zero = Num 0
one  = Num 1
negOne = Num $ -1

neg :: ExpTree -> ExpTree
neg = Op "*" negOne

inv :: ExpTree -> ExpTree
inv = Op "/" (Num 1)

isConst :: ExpTree -> Bool
isConst (Num _)  = True
isConst (Op _ l r) = (isConst l) && (isConst r)
isConst (Fun _ m) = (isConst m)
isConst         _  = False

noncommutative :: String -> Bool
noncommutative op = op /= "+" && op /= "*"

instance Show ExpTree where
	show (Num const) = if const >= 0 then show const else "(" ++ show const ++ ")"
	show (Var var) = var
	show (Fun name m) = name ++ "(" ++ show m ++ ")"
	show op@(Op s l r) = ls ++ s ++ rs
		where
			ls = if priority2 op > priority2 l || leftPow op  then "(" ++ show l ++ ")" else show l
			rs = if priority2 op > priority2 r || rightNonCom then "(" ++ show r ++ ")" else show r
			rightNonCom = noncommutative s && priority2 op >= priority2 r
			leftPow (Op "^" (Op "^" _ _) _) = True -- Because of right assosiation
			leftPow _ = False


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
derivateTree var (Num _) = Num 0
derivateTree var (Var name) 
	| name == var = Num 1
	| otherwise   = Num 0
derivateTree var f@(Fun name m)
	| name == "sin" = (Fun "cos" m) *.* dm
	| name == "cos" = (neg (Fun "sin" m)) *.* dm
	| name == "log" = dm /./ m
	| name == "exp" = f *.* dm
	where
		dm = derivateTree var m
derivateTree var (Op name l r)
	| name == "+" = dl +.+ dr
	| name == "-" = dl -.- dr
	| name == "*" = dl *.* r +.+ l *.* dr
	| name == "/" = (dl *.* r -.- l *.* dr) /./ (r ^.^ (Num 2))
	| name == "^" = r *.* l ^.^ (r -.- Num 1) *.* dl +.+ (ln l)*.* l ^.^ r  *.* dr
	where
		dl = derivateTree var l
		dr = derivateTree var r
		ln = Fun "log" 


simplifyConst :: ExpTree -> ExpTree
simplifyConst (Op "*" l r) | l == zero  = zero
simplifyConst (Op "*" l r) | r == zero  = zero
simplifyConst (Op "*" l r) | l == one   = r
simplifyConst (Op "*" l r) | r == one   = l
simplifyConst (Op "+" l r) | l == zero  = r
simplifyConst (Op "+" l r) | r == zero  = l
simplifyConst (Op "-" l r) | l == zero  = neg r
simplifyConst (Op "-" l r) | r == zero  = l
simplifyConst (Op "/" l r) | l == zero  = zero
simplifyConst (Op "^" l r) | l == zero  = zero
simplifyConst (Op "^" l r) | r == zero  = one
simplifyConst (Op "^" l r) | l == one   = one
simplifyConst (Op "^" l r) | r == one   = l
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
