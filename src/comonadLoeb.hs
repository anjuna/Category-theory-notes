{-# LANGUAGE DeriveFunctor #-}

-- import Control.Comonad

loeb :: Functor f =>  f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs

fix :: (a -> a) -> a 
fix f = let x = f x in x

loebNew :: Functor f =>  f (f a -> a) -> f a
loebNew fs = fix $ \xs -> fmap ($ xs) fs

-- hmmmm

-- cant load package for some reason, here's the typeclass anyway

class (Functor w) => Comonad w where
    coreturn :: w a -> a 
    cojoin :: w a -> w (w a)
    (=>>) :: w a -> (w a -> b) -> w b
    x =>> f = fmap f $ cojoin x
    


data Stream a = Cons a (Stream a) deriving Functor


-- eg real number line stretching -Infinity to +Infinity
data Tape a = Tape (Stream a) a (Stream a) deriving Functor

instance Comonad Tape where
    coreturn Tape (l x r) = x
    cojoin 









