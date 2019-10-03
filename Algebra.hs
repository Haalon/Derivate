{-# LANGUAGE ExtendedDefaultRules #-}

module Algebra where

import Expression

default (Double)

algebraSyn :: [Lex Double]
algebraSyn = 
    [ makeFun "sin" sin 1
    , makeFun "cos" cos 1
    , makeFun "logBase" logBase 2
    , makeFun "log" log 1
    , makeFun "exp" exp 1
    , makeFun "asin" asin 1
    , makeFun "acos" acos 1
    , makeFun "atan" atan 1
    , makeOp "+" (+) 4 InL
    , makeOp "*" (*) 3 InL
    , makeOp "/" (/) 3 InL
    , makeOp "-" (-) 4 InL
    , makeOp "^" (**) 2 InR
    , makeOp "-" (0-) 1 Pre
    -- , makeOp "!" 0 Post
    , makeConst "pi" pi
    , makeConst "e" $! exp 1
    ]