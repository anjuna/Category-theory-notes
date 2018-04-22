-- inspired from:
-- http://www.stephendiehl.com/posts/adjunctions.html
-- https://bartoszmilewski.com/2016/04/18/adjunctions/
-- https://en.wikipedia.org/wiki/Adjoint_functors

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}


module Adjunctions where


adjunctionMain :: IO ()
adjunctionMain = putStrLn "Adjunctions are working!"

-- Want to show adjunction between product and reader functors, and how this elicits currying

data MyProduct a b = P { first :: a, second :: b }

data MyReader a b = R {runReader :: a -> b }

data MyIdentity a = I { unI :: a }

instance Functor (MyProduct a) where
    fmap f p = P (first p) (f $ second p)

instance Functor (MyReader a) where
    fmap f r = R $ f . runReader r

instance Functor (MyIdentity) where
    fmap f (I x) = I $ f x


data CompF f g a = CompF { unCompF :: f (g a) }

-- (a -> b) -> f (g a) -> f (g b)
instance  (Functor f, Functor g) => Functor (CompF f g) where
    fmap fun c = CompF $ fmap (fmap fun) (unCompF c)
    
-- the component at x of the natural transformation between functors f and g
type f ~> g = forall x . f x -> g x



-- F ⊣ G
class (Functor f, Functor g) => MyAdjunction f g where
    -- 'extract' for a comonad
    epsilon :: CompF f g ~> MyIdentity
    -- exactly 'return' in monad
    eta :: MyIdentity ~> CompF g f


-- Need to find natural transformations:
instance MyAdjunction (MyProduct a) (MyReader a) where
    epsilon (CompF (P i (R r))) = I $ r i
    eta (I x) = CompF $ R $ \e -> P e x

-- Wanted to show composition and identity functions with the data types
-- these just unwrap the adjunction transformations
easierEpsilon :: MyAdjunction f g => forall z . f (g z) -> z
easierEpsilon = unI . epsilon . CompF
easierEta :: MyAdjunction f g => forall z . z -> g (f z)
easierEta = unCompF . eta . I


-- These natural transformations can induce the adjunction via hom sets:
-- For each f : FY → X and each g : Y → GX, define
phi :: MyAdjunction f g => forall x y . (f y -> x) -> (y -> g x)
psi :: MyAdjunction f g => forall x y . (y -> g x) -> (f y -> x)

-- Want to show phi . psi = psi . phi = id
phi f = fmap f . easierEta
psi g = easierEpsilon . fmap g


-- hmm
-- let right = phi . psi
-- let left = psi . phi


class (Functor t) => MyMonad t where
    muM :: t (t a) -> t a
    etaM :: a -> t a
    (>>=) :: t a -> (a -> t b) -> t b
    x >>= f = (muM . fmap f) x

-- lets make the monad from two adjoint functors:

class (MyAdjunction f g) => Monad m where
    etaA :: eta
    













