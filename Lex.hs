{-# LANGUAGE ScopedTypeVariables #-}
module Lex where

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

instance Eq (Lex a) where
    (==) l1 l2 = lex_token l1 == lex_token l2 && lex_class l1 == lex_class l2


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