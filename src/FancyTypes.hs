{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module FancyTypes where

data Nat = Zero | Succ Nat

-- GADT for lenghth indexed vectors
data VecN :: * -> Nat -> * where
    (:>) :: a -> VecN a n -> VecN a ('Succ n)
    VNil :: VecN a 'Zero


type family n + m where
    'Zero + m = m 
    'Succ n + m = 'Succ (n + m)
