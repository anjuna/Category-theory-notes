-- "A lens is a comonad coalgebra"

-- Given a 'whole', retrieve for me a part
-- Given a 'whole' and new part, instert that part, returning the new whole
-- databases yo

-- get :: w -> p
-- set :: w -> p -> w

-- Both kind of expressed in w -> (p, p -> w).
-- It's functorial in w:

data MyStore p w = MyStore p (p -> w)

class Lensy l where
    get :: a -> b 
    set :: a -> b -> a

instance Functor (MyStore p) where
    fmap fun (MyStore p f) = MyStore p (fun . f)

-- lens laws:

-- If you get something and set it, nothing's changed:
-- A ===>  set w (get w) = id

-- If you set x, then get it, it's the same as x:
-- B ===> get (set w p) = p

-- If you compose setting things, the last application sticks:
-- C ===> set (set w p) p' = set w p'

-- it's also a Comonad:



instance Comonad (MyStore p) where
    -- extract :: w a -> a
    extract MyStore p f = f p
    -- duplicate :: w a -> w (w a)
    duplicate MyStore p f = MyStore p (\q -> MyStore q f)

-- With a comonad we might have a coalgebra:

type MyCoalgebra w a = a -> w a














