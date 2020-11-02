# Pure Operators

($) :: (a -> b) -> a -> b
Function application with `$`. Low precedence and right associative. I.e. `f (g (z x))` can be written `f $ g $ z x`.

(.) :: (b -> c) -> (a -> b) -> a -> c
Function composition with `.`. Right associative. I.e. `f (g (z x))` equals `(f . g . z) x`.

## Functor Typeclass
fmap :: (a -> b) -> f a -> f b
Equivalent to `map` in Scala. 

## Applicative Typeclass
pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
Basically `fmap` as infix operator. `f <$> x` is the same as `fmap f x`

## Monad Typeclass
return :: a -> m a
Basically `pure`

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
Equivalent to `flatMap` in Scala.

(>>) :: m a -> m b -> m b
Equivalent to '*>' in Scala cats.

fail :: String -> m a
Raise error in 'm'

## X Monad ;)
io :: MonadIO m => IO a -> m a

# Structural 

## Pattern Matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

## Guards
max' :: (Ord a) => a -> a -> a
max' a b 
  | a > b = a
  | otherwise = b

## Where
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

## Let
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in sideArea + 2 * topArea

## Case
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "single"
                                               xs -> "multiple"

## Monad 'do' notation
Use to unwrap/bind monad values to variables to be reused. Similar to Scala 'for' comprehension.

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)
