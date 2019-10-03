{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Chain where

data Chain a = Head a | Tail (a -> Chain a)

-- instance Functor Chain where
--     -- fmap :: (a -> b) -> Chain a -> Chain b
--     fmap f (Head val) = Head $ f val
--     fmap f (Tail fun) = Tail (\v -> fmap f (fun v))

class Chainable a r where 
    chain :: r -> Chain a

instance Chainable a a where
    chain = Head    

instance Chainable a r => Chainable a (a -> r) where
    chain f = Tail (\v -> chain $ f v)


feed :: Chain a -> a -> Chain a
feed (Tail f) val = f val
feed chain _ = chain

feedList :: Chain a -> [a] -> Chain a
feedList = foldl feed

unChain :: Chain a -> Maybe a
unChain (Head val) = Just val
unChain _ = Nothing

applyList :: Chainable a r => r -> [a] -> Maybe a
applyList f ls = unChain $ foldl feed (chain f) ls