module Combinators where

s :: (a -> b -> c) -> (a -> b) -> a -> c 
s x y z = (x z) (y z)

-- ie the constant function, throwing away whatever context it had
k :: a -> b -> a 
k x = \_ -> x

-- f a = r -> a
data MyArrow r a = MyArrow {runArrow :: (r -> a)}

instance Functor (MyArrow r) where
    fmap f (MyArrow g) = MyArrow (f . g)

instance Applicative (MyArrow r) where
    -- pure :: a -> f a
    -- pure :: a -> (r -> a)
    pure x = MyArrow (k x)

    -- ie pure = k

    -- (<*>) :: f (a -> b) -> f a -> f b
    -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b) 
    (<*>) (MyArrow x) (MyArrow y) = MyArrow (s x y)

    -- ie (<*>) = s








