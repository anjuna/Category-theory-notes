{-# LANGUAGE DeriveFunctor #-}

module ComonadStuff where

-- import Control.Comonad

loeb :: Functor f =>  f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs

-- observe ($) :: (a -> b) -> a -> b
-- then ($ a) :: (a -> b) -> b (partially applying the right argument to the infix operator)
-- ie  ($ f) = \x -> f $ x
-- think of it as a 'suspended computation', but more on that in CPS notes
loebReadable :: Functor f =>  f (f a -> a) -> f a
loebReadable fs = xs where xs = fmap (\f -> f xs) fs

fix :: (a -> a) -> a 
fix f = let x = f x in x

loebNew :: Functor f =>  f (f a -> a) -> f a
loebNew fs = fix $ \xs -> fmap ($ xs) fs

-- without the weird continuation thing ($ xs)
loebAgain :: Functor f =>  f (f a -> a) -> f a
loebAgain fs = fmap (\f -> f (loebAgain fs)) fs

-- hmmmm

-- cant load package for some reason, here's the typeclass anyway
class (Functor w) => Comonad w where
    coreturn :: w a -> a 
    duplicate :: w a -> w (w a)
    extend :: w a -> (w a -> b) -> w b
    extend x f = fmap f $ duplicate x
    


data Stream a = Cons a (Stream a) deriving Functor

head :: Stream a -> a 
head (Cons x _) = x 

tail :: Stream a -> Stream a 
tail (Cons _ xs) = xs

-- useful for seeing things in repl
takeN :: Int -> Stream a -> [a]
takeN n (Cons x xs) = if n == 0 then [] else (x:(takeN (n - 1) xs))


-- eg real number line stretching -Infinity to +Infinity with 0 in the middle
data Tape a = Tape { left :: Stream a, focus :: a, right :: Stream a} deriving Functor

-- move the focus to the left
moveLeft :: Tape a -> Tape a
moveLeft (Tape (Cons lh lt) f r) = Tape lt lh (Cons f r)

-- move the focus to the right
moveRight :: Tape a -> Tape a
moveRight (Tape l f (Cons rh rt)) = Tape (Cons f l) rh rt

iterStream :: (a -> a) -> a -> Stream a
iterStream f e = let eAgain = f e 
                in Cons e (iterStream f eAgain)

instance Comonad Tape where
    coreturn (Tape _ x _) = x
    duplicate t = Tape (iterStream moveLeft t) t (iterStream moveRight t)








