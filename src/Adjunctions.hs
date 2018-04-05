{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Adjunctions where


-- Want to show adjunction between product and reader functors, and how this elicits currying

data MyProduct a b = P { first :: a, second :: b }

data MyReader a b = R {runReader :: a -> b }

instance Functor (MyProduct a) where
    fmap f p = P (first p) (f $ second p)

instance Functor (MyReader a) where
    fmap f r = R $ f . runReader r

type f ~> g = forall x . f x -> g x
-- Need to find natural transformations: