{-|
Module      : Main
Description : Main module, running script.
Copyright   : Just Nothing
Stability   : Stable
-}
import System.Environment
import Data.Char
import Data.Maybe
import Data.List hiding(insert)
--import qualified Data.Sequence as Seq

--data Op = Sum | Mul | Pow | Div | Sub
--data Fun = Sin | Cos | Log | Exp

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

-- for RPN
data Token = Const Double | Name String | BinOp String | UnOp String | OpBr | ClBr | End
	deriving(Eq)

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

{-
data Op = Sum | Mul
	deriving(Show,Eq,Ord)

getOp Mul = (*)
getOp Sum = (+)

getNeutral Mul = 1
getNeutral Sum = 0

data Exp = Grp Op Double [Exp] | Pow Exp Exp | Fun String Exp | Var String | Num Double
	deriving(Show)

-- similar expressions -- expressions that can be added together
(=.=) :: Exp -> Exp -> Bool 
(=.=) (Var v1)(Var v2)              = v1 == v2
(=.=) (Num n1)(Num n2)              = True -- Nums are always similar
(=.=) (Fun f1 e1)(Fun f2 e2)        = f1 == f2 && e1 =.= e2
(=.=) (Pow e11 e12)(Pow e21 e22)    = e11 =.= e21 && e12 =.= e22
--(=.=) (Mul _ es1) (Mul _ es2)     = (and $ map (\x -> or $ map (=.= x) es2) es1) && (and $ map (\x -> or $ map (=.= x) es1) es2)
(=.=) (Grp o1 _ es1) (Grp o2 _ es2) = o1==o2 && length es1 == length es2 && (and $ map (\x -> or $ map (=.= x) es2) es1)
(=.=) _ _                           = False



-- used only if Terms are similar
combine :: Exp -> Exp -> Exp
combine (Grp op1 n1 es1) (Grp _ n2 _) = Grp op1 ((getOp op1) n1  n2) es1
combine a             _               = a

insert :: Exp -> [Exp] -> [Exp]
insert e es = case findIndex (e =.=) es of 
	Nothing -> es ++ [e] -- position?
	Just i -> front ++ [combine e elem] ++ end
		where
			(front, elem:end) = splitAt i es


merge :: [Exp] -> [Exp] -> [Exp]
merge es1 []  = es1
merge es1 es2 = foldr insert es1 es2


(+..+) :: Exp -> Exp -> Exp
(+..+) (Grp Sum n1 es1) (Grp Sum n2 es2) = Grp Sum (n1+n2) $ merge es1 es2
(+..+) (Grp Sum n1 es1) (Num n2)         = Grp Sum (n1+n2) es1
(+..+) (Num n1) (Grp Sum n2 es2)         = Grp Sum (n1+n2) es2
(+..+)	e1 e2 = Grp Sum 0 [e1,e2]

(*..*) :: Exp -> Exp -> Exp
(*..*) (Grp Mul n1 es1) (Grp Mul n2 es2) = Grp Mul (n1*n2) $ merge es1 es2
(*..*) (Grp Mul n1 es1) (Num n2)         = Grp Mul (n1*n2) es1
(*..*) (Num n1) (Grp Mul n2 es2)         = Grp Mul (n1*n2) es2
(*..*)	e1 e2 = Grp Mul 1 [e1,e2]

test1 = Grp Sum 1 [y, Pow x $ Num 2]
test2 = Grp Sum 2 [y, Pow x $ Num 2]
test3 = Grp Mul 1 [y, Pow x $ Num 2]
test4 = Grp Mul 2 [y, Pow x $ Num 2]
x = Var "x"
y = Var "y"


(-..-) = Op "-"
(/../) = Op "/"
(^..^) = Op "^"
-}

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



instance Show ExpTree where
	show (Num const) = if const >= 0 then show const else "(" ++ show const ++ ")"
	show (Var var) = var
	show (Fun name m) = name ++ "(" ++ show m ++ ")"
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

polishInverse :: String -> [Token] -> [Token]
polishInverse expr stack = case parseToken expr of 
	(tok@(Name _), rest) -> tok: (polishInverse rest stack)
	(tok@(Const _), rest) ->tok: (polishInverse rest stack)
	(tok@(UnOp _), rest) ->polishInverse rest $ tok:stack
	(tok@(OpBr), rest) ->polishInverse rest $ tok:stack
	(tok@(ClBr), rest) ->add ++ (polishInverse rest (tail left))
		where
			(add,left) = span (not.isOpBr) stack	
	(tok@(BinOp op), rest) -> if op == "-" && (length stack == 0 || OpBr == head stack )
		then (Const $ -1) : (polishInverse ('*' :rest) stack)
		else add ++ (polishInverse rest (tok:left))
			where
				(add,left) = span (\x -> priority tok < priority x) stack
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

-- - (a + b) -- ab+(-1)*
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
derivate var expr = show  $ simplify $ derivateTree var $ createTree (polishInverse expr []) []

derivate' :: String ->  String -> String
derivate' var expr = show  $ derivateTree var $ createTree (polishInverse expr []) []

smp expr = simplify $ createTree (polishInverse expr []) []


main :: IO ()
main = do
  putStrLn "For the Great Good!";
  a <- getArgs; 
  print $ head a
