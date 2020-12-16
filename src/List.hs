import Control.Monad (liftM, ap)

data LinkedList a = Nil | Cons a (LinkedList a)
  deriving Show

lcat :: LinkedList a -> LinkedList a -> LinkedList a
lcat Nil list = list
lcat (Cons head tail) list = Cons head (lcat tail list)

lflatten :: LinkedList (LinkedList a) -> LinkedList a
lflatten Nil = Nil
lflatten (Cons head tail) = lcat head (lflatten tail)

lmap :: (a -> b) -> LinkedList a -> LinkedList b
lmap f Nil = Nil
lmap f (Cons head tail) = Cons (f head) (lmap f tail)

instance Functor LinkedList where
  fmap = liftM

instance Applicative LinkedList where
  pure = return
  (<*>) = ap

instance Monad LinkedList where
  return x = Cons x Nil
  xs >>= f = lflatten $ lmap f xs

data Point = Point Int Int 
  deriving Show

neighbors :: Point -> LinkedList Point
neighbors (Point x y) = (Cons left (Cons right (Cons top (Cons bottom Nil))))
  where
    left = Point (x - 1) y
    right = Point (x + 1) y
    top = Point x (y + 1)
    bottom = Point x (y - 1)

-- 3rd generation neighbors
main = print $ neighbors (Point 0 0) >>= neighbors >>= neighbors