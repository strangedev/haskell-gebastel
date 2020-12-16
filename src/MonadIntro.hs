import Control.Monad (liftM, ap)
-- A Monad in the functional programming sense is a sort of container
-- type. Let's for example assume we've got some type A, but
-- we want to store some additional data alongside it.
-- We do this, by augmenting a with some monad M, which contains
-- this information.

newtype M a = M a

-- For now, we will look at this from a purely type-based point
-- of view. M in itself does not contain any data yet, we
-- will do this later.
-- For now, it is sufficient that observe what happens at the
-- type level.

-- We may now wrap any value of any type in an M.
-- This changes the type from a to (M a).

-- Footnote: For the sake of being able to do anything useful with a,
-- we constrain a to mean any real number here.

wrapped :: (Real a) => M a
wrapped = M 2

-- Let's say now, that we've got some function f, which operates
-- on a and returns a value of the same type. 

f :: (Real a) => a -> a
f a = 2 * a

-- Wouldn't it be nice to apply this function to our wrapped value?
-- How would that look?

applyToWrapped :: M a -> (a -> a) -> M a
applyToWrapped (M a) f = M (f a)

appliedToWrapped :: (Real a) => M a
appliedToWrapped = applyToWrapped wrapped f

-- This is really nice!
-- As we can see, augmenting a to contain extra data changes its
-- type - a becomes M a - but we are still able to apply our
-- normal functions on it.
-- f does not know of the existence of M!
-- Note also, that our applyToWrapped function does not need to know the
-- type of a!

-- But what if we have instead a more complicated function, which does not
-- return the same type? The pattern looks quite similar:

g :: (Real a, Show a) => a -> String
g = show

applyToWrapped2 :: M a -> (a -> b) -> M b
applyToWrapped2 (M a) f = M (f a)

appliedToWrapped2 = applyToWrapped2 wrapped g

-- Notice that the function body is the same!
-- What this means is, that we may wrap any type a with some additional data
-- without losing the functions that apply to a.

-- But what about composition? Say we want to combine f and g:
h :: (Real a, Show a) => a -> String
h = g . f

appliedToWrapped3 :: M String
appliedToWrapped3 = applyToWrapped2 wrapped h

-- That works as well!

-- Now towards something more useful. We are now going to
-- actually add some data to M. Let's say that we are
-- trying to model errors in our functional program.
-- We might use a type like this:

data Result tData = Failure String | Success tData
  deriving (Show)

-- Here, we swap M for Result and a for tData.
-- We can read this type as saying:
-- A Result of type tData is either a Failure or a Success.
-- In the case of a Failure, it includes an error message.
-- Otherwise, it contains some data of type tData.
-- Notice that we have augmented to type tData by wrapping it
-- in a Result - just as we have augmented a by wrapping it in
-- an M.

-- Let us define some ordinary functions for our tData, which will
-- be a Real once again:

triple :: (Real a) => a -> a
triple a = 3 * a

quadruple :: (Real a) => a -> a
quadruple a = 4 * a

-- As expected, we can now apply the triple function to a value
-- wrapped in the Result type:

applyToResult :: Result a -> (a -> b) -> Result b
applyToResult (Success a) f = Success (f a)
applyToResult (Failure msg) _ = Failure msg

appliedToResult :: Result Integer
appliedToResult = applyToResult (Success 2) triple


-- Notice that we now have to decide what to do with the additional data!
-- We have to decide what it means to apply a function to a failed result.
-- In this case, we choose to propagate the failure.
-- Notice again, that the function we apply to our Result a may change the type
-- of a!

appliedToResult2 :: Result String
appliedToResult2 = applyToResult (Success 2) show

appliedToResult3 :: Result Integer
appliedToResult3 = applyToResult (Failure "Oopsies!") (triple . quadruple)

-- This is another very powerful thing! We have defined what it means
-- to apply a function to a result - regardless of what type the result is
-- or what the function does. We can now apply any function to a result
-- and have the additional data - i.e. the error message - handled correctly.

-- At the moment, the example is not very useful. Unless we stick a Failure
-- into our applicator, we will never get anything else but a Success,
-- regardless of how we compose the applied functions.
-- The real power comes from allowing every function in our chain of
-- composition to "raise" a Failure:

doNotApplyToThree :: (Real a) => a -> Result a
doNotApplyToThree 3 = Failure "I heckin told ya!"
doNotApplyToThree a = Success a

-- How do we apply this function to a Result?

bind :: Result a -> (a -> Result b) -> Result b
bind (Success a) f = f a
bind (Failure msg) _ = Failure msg

applyWithBind :: Result Integer
applyWithBind = bind (Success 2) doNotApplyToThree

applyWithBind2 :: Result Integer
applyWithBind2 = bind (Success 3) doNotApplyToThree

-- Bind is a really powerful tool. In fact, it
-- is one of the axioms that make a type monadic.
-- It doesn't only allow us to apply a function that returns a wrapped type
-- to a wrapped type, we can also compose these functions with it!

doNotApplyToEight :: (Real a) => a -> Result a
doNotApplyToEight 8 = Failure "I heckin told ya!"
doNotApplyToEight a = Success a

bindIsCompose :: Result Integer
bindIsCompose = Success 2 `bind` doNotApplyToThree `bind` doNotApplyToEight

-- Now we've got a way to compose functions for plain types a,
-- a way to wrap these plain types to convey additional data M a,
-- a way to compose function on plain types (that's just . ),
-- and a way to compose functions, that take plain types and return
-- wrapped types.
-- Wouldn't it be great to combine all this into one cohesive way
-- of dealing with plain and wrapped types?
-- How do we make plain functions compatible with our new and fancy
-- wrapped functions?

-- In the same way in which we defined how the failure is handled
-- in the bind function, we now need to define a sort of "standard"
-- representation for our plain types using the wrapper.
-- Suppose we take the following function and want to make it compatible
-- with our Result wrapper:

k :: (Real a) => a -> a
k a = 42 * a

-- We could do this by defining all plain values to be a successful result:

wrap :: (t -> tData) -> t -> Result tData
wrap f a = Success (f a)

kWrapped :: Integer -> Result Integer
kWrapped = wrap k

-- Now, we may use any function, from any type to any other, raising
-- an error or not, to compose freely with!

thisIsAwesome :: Result Integer
thisIsAwesome = Success 2 `bind` doNotApplyToEight `bind` wrap f `bind` doNotApplyToThree

-- What do we get by this?
-- 1. We are able to define how a failure is handled once and reuse this approach.
-- 2. We are able to compose failure-aware and plain functions while preserving the failure information.

-- This approach is in my opinion best explained using the notion of failure,
-- since it models how exceptions work in imperative programming.
-- It is, however, much more powerful than this.
-- By specifying how values are wrapped into our container
-- and how the additional data is handled when composing,
-- we can apply this approach to many things.
-- It enables us to write pure programs and later add on other notions of
-- computing, without polluting our pure functions with knowledge of this
-- outside world.
-- In fact, this approach is so common, that it has its own typeclass:


instance Functor Result where
  fmap = liftM

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  (Failure msg) >>= _ = Failure msg
  (Success value) >>= f = f value
  return = Success

-- >>= is what we previously called bind
-- return is our wrap operator

thisIsFine :: Result Integer
thisIsFine = Success 2 >>= doNotApplyToThree >>= (return . quadruple)

thisIsBad :: Result Integer
thisIsBad = Success 2 >>= doNotApplyToThree >>= (return . quadruple) >>= doNotApplyToEight