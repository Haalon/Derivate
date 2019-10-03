{-# LANGUAGE ScopedTypeVariables #-}
module Expression where

import Data.List
import Data.Maybe
import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import Parser
import Chain

data Assoc = InL | InR | Pre | Post | None
    deriving(Eq,Show)

data LexClass = Op | Fun | Const | Var
    deriving (Eq, Show)

data Lex a = Lex 
    { lex_token :: String
    , lex_value :: Chain a
    , lex_prior :: Int
    , lex_arity :: Int
    , lex_assoc :: Assoc
    , lex_class :: LexClass
    }

instance Show (Lex a) where
    show lx = show cls ++ " " ++ show tok
      where 
        tok = lex_token lx
        cls = lex_class lx

-- scope 'a' type variable for chain
defaultLex :: forall a. Lex a
defaultLex = Lex
    { lex_token = ""
    , lex_value = chain (id :: a -> a)
    , lex_prior = 10
    , lex_arity = 0
    , lex_assoc = None
    , lex_class  = Op
    }

makeOp :: forall a f. Chainable a f => String -> f -> Int -> Assoc -> Lex a
makeOp tok val pr assc = defaultLex 
    { lex_token = tok
    , lex_value = chain val :: Chain a
    , lex_prior = pr
    , lex_arity = arity
    , lex_assoc = assc
    , lex_class = Op
    }
  where
    arity = if assc == Post || assc == Pre
        then 1
        else 2

makeFun :: forall a f. Chainable a f => String -> f -> Int -> Lex a
makeFun tok val arity = defaultLex 
    { lex_token = tok
    , lex_value = chain val :: Chain a
    , lex_arity = arity
    , lex_class = Fun
    }

makeConst :: forall a. String -> a -> Lex a
makeConst tok val = defaultLex 
    { lex_token = tok
    , lex_value = chain val :: Chain a
    , lex_class = Const
    }

makeVar :: String -> Lex a
makeVar tok = defaultLex 
    { lex_token = tok
    , lex_class = Var
    }


prior_table :: [Lex a]-> [[Lex a]]
prior_table syn = raw_table
    where
        ops = filter ((==Op). lex_class) syn
        minp = minimum . (map lex_prior) $ ops
        maxp = maximum . (map lex_prior) $ ops
        layer n = filter ((==n) . lex_prior) $ ops
        raw_table = filter (not.null) [layer k | k <- [maxp, maxp-1..minp]]
        -- check row = 

data AST a = AST (Lex a) [AST a]
    -- deriving(Eq)

-- -- | Neat 2-dimensional drawing of a tree.

drawFlat :: Int -> AST a  -> String
drawFlat pr (AST lex trees) = str
  where
    lclass = lex_class lex
    lassoc = lex_assoc lex
    lprior = lex_prior lex
    tok = lex_token lex
    parentize str' = if pr < lprior
        then "(" ++ str' ++ ")"
        else str'
    substrs = map (drawFlat lprior) trees
    str = case lclass of 
        Fun -> tok ++ "(" ++ intercalate "," substrs ++ ")"
        Const -> tok
        Var -> tok
        Op -> case lassoc of
            Pre -> parentize $ tok ++ head substrs
            Post -> parentize $ head substrs ++ tok
            InL -> parentize $ intercalate tok substrs
            InR -> parentize $ intercalate tok substrs
            None -> parentize $ intercalate tok substrs
       



drawAST :: AST a -> String
drawAST  = unlines . draw

draw :: AST a -> [String]
draw (AST x ts0) = (show x) : drawSubTrees ts0
  where 
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "\\_ " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "|> " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

instance Show (AST a) where
    show = drawAST


-- -- dummy = AST "dummy" []

type ASTParser a = Parser [Lex a] (AST a)

merge :: Lex a->AST a->AST a->AST a
merge lx l r = AST lx [l,r]

apply :: Lex a->AST a->AST a
apply lx f = AST lx [f]

blank :: Lex a->AST a
blank lx = AST lx []

lexP :: Lex a -> Parser r (Lex a)
lexP lx = reserved tok *> return lx
  where
    tok = lex_token lx

entryP :: Parsable a => ASTParser a
entryP = do
    spaces *> levelP 0 <* spaces -- <* eof

levelP :: Parsable a => Int -> ASTParser a
levelP i = do
    t <- asks prior_table
    let next' = if length t == i+1 then finalP else levelP (i+1)
    let next = spaces *> next' <* spaces
    let ops = t !! i
    let asc = lex_assoc . head $ ops
    case asc of
        InL -> chainl1 next (choice $ (fmap merge . lexP) <$> ops)
        InR -> chainr1 next (choice $ (fmap merge . lexP) <$> ops)
        Pre -> chain0 (choice $ (fmap apply . lexP) <$> ops) <*> next
        Post -> do
            term <- next
            chain0 (choice $ (fmap apply . lexP) <$> ops) <*> return term

finalP :: Parsable a => ASTParser a
finalP = do
    funs <- asks $ filter ((==Fun). lex_class)
    consts <- asks $ filter ((==Const). lex_class)
    let varP = blank <$> makeVar <$> alphaNum
    let namedConstP lx = blank <$> (lexP lx)
    (choice $ funcP <$> funs)
        <|> (choice $ namedConstP <$> consts)
        <|> (parens "(" ")" entryP)
        <|> constP
        <|> varP


constP :: Parsable a => ASTParser a
constP = do
    before <- look
    val <- parse
    after <- look
    let repr = take (length before - length after) before
    return $ blank $ makeConst repr val

funcP :: Parsable a => Lex a -> ASTParser a
funcP lx = do
    let ar = lex_arity lx
    let inner1 = (:) <$> entryP <*> count (ar-1) (reserved "," >> entryP)
    let inner = if ar > 0
        then inner1
        else return []
    fun <- lexP lx
    ls <- parens "(" ")" inner
    return $ AST lx ls