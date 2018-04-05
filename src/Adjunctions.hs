{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

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


type f ~> g = forall x . f x -> g x
data CompF f g a = CompF { unCompF :: f (g a) }

-- (a -> b) -> f (g a) -> f (g b)
instance  (Functor f, Functor g) => Functor (CompF f g) where
    fmap fun c = CompF $ fmap (fmap fun) (unCompF c)
    

-- F ⊢ G
class (Functor f, Functor g) => MyAdjunction f g where
    -- eta
    unit :: CompF f g ~> MyIdentity
    -- epsilon
    counit :: MyIdentity ~> CompF g f


-- Need to find natural transformations:



















