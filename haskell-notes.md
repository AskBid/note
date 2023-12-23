```haskell
{- TERMINOLOGY / ANATOMY -}
func :: a -> a -- < [TYPE]
func a = a
     --[TERM]
func a = undefined 
     --[bottomed out TERM]


{-<-}

data Compass = North | East | South | West

instance Show Compass where
  show North = "North"
  show East = "East"
  show South = "South"
  show West = "West"

instance Eq Compass where 
  North == North = True
{->-}

{->-}
data Compass = North | East | South | West
  deriving (Show, Eq, Ord, Enum)
{->-}

{-<-}

data Expr = Nub Int | Add Expr Expr | Sub Expr
  deriving (Show, Eq)

calc :: Expr -> Int
calc (Nub x) = x
calc (Add x y) = (calc x) + (calc y)
calc (Sub x) = (calc x)

{->
*Main> calc (Add (Nub 1) (Sub (Nub 3)))
4
-}

{-
stack new hello-world simple
stack ghci                --inside prj dir then project is running in console, :r to reload
stack build
stack exec hello-world
stack runghc program.hs   --interpreter
stack ghc program.hs      --compiler
:!dir
:!ls
:cd ..
:set -Wall 
  -Wall flag in our REPL (or in our build configuration), then
  GHC will let us know when we’re not handling all cases.
-}

{- DEBUGGER
stack ghci
:l program.hs
:break func

:ghci program.hs
:step func
-}

{- composition -}
f x = x + 1
g x = x / 3
(f.g) 5 -- 2.666
(g.f) 5 -- 2.0
-- =
g (f 5)

{--}

{- composition -}
mapFunc = map func4each ['a','r','r','a','y']
foldrFunc = foldr (+) 5 [1,2,3,4] -- => 15
{--}

{--}
() --unit value (like empy tuple) that means the function returns nothing
{--}

{--}
content <- readFile "numbers.txt"
--p
{--}

{- Lambda -}
(\x y -> x + y) 3 5
lst = map (\x -> x + 1) lst
{--}

{--}
parenthesizeWords s = unwords $ map parenthesizeWord (words s)
  where parenthesizeWord word = "(" ++ word ++ ")"
--
parenthesizeWords s = unwords $ map (\x -> "(" ++ x ++ ")") (words s)
{-Composition-}
parenthesizeWord = ("(" ++) . (++ ")")
{--}

{-Sections-}
foo x y z = x ++ y ++ z
foo "aaa" "bbb" "ccc"
-- -> "aaabbbccc"
--  > :t foo
-- -> foo :: [a] -> [a] -> [a] -> [a]
x = foo "aaa"
--  > :t x
-- -> x :: [Char] -> [Char] -> [Char]
y = x "bbb"
--  > :t y
-- -> y :: [Char] -> [Char]
z = y "ccc"
--  > :t z
-- -> y :: [Char]
-- > z
-- -> "aaabbbccc"

--
map (*2) $ filter (<5) [1..10]
-- -> [2,4,6,8]
{--}

{-TYPES -}
type Port = Int --just an alias
type HostInfo = (String, Port)

f :: Num a => a -> b -> Int -> Int
--           [0]  [1]   [2]    [3]
-- What type variable/constructor?: 
-- constrained polymorphic (Num) ([0]), 
-- fully polymorphic ([1]), 
-- concrete type ([2] and [3]).

{-----}
-- what the '_' will wildcard is the * -> [a] -> b down here
myFold :: (a -> b -> b) -> b -> [a] -> b
myFold _ b [] = b
myFold f b (a : as) = myFold f (f a b) as

mySum :: _ -- << wildcard
mySum = myFold (+) 0

main :: IO ()
main = print $ mySum [5, 6, 7] 
-- Found type wildcard ‘_’ standing for ‘[Integer] -> Integer’
main = print $ mySum [5.0, 0.3, 4] 
-- Found type wildcard ‘_’ standing for ‘[Double] -> Double’
main = print $ mySum [5 :: Int]
-- Found type wildcard ‘_’ standing for ‘[Int] -> Int’
{---}
myFold :: a -> b -> b
myFold a b = a + b 

mySum :: _ -- << wildcard
mySum = (+) 0.1

main :: IO ()
main = print $ mySum 5
-- Found type wildcard ‘_’ standing for ‘[Double] -> Double’
{----}

{-- ALGEBRAIC DATATYPES --}
data Colour = Red | Green | Blue deriving Show
data Colour = RGB Int Int Int deriving Show

data Colour = RGB { red :: Int, green :: Int, blue :: Int } deriving Show
x = RGB 10 20 30
x 
-- -> RGB {red =10, green = 20, blue = 30}
red x
-- -> 10
green x
-- -> 20
blue x
-- -> 30
y = x {green = 40}
x
-- -> RGB {red =10, green = 20, blue = 30}
y
-- -> RGB {red =10, green = 40, blue = 30}
data Colour = RGB Int Int Int | CMYK Float Float Float Float deriving Show

{-- ALGEBRAIC DATATYPES --}
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

{-- CLASSTYPES --}
-- ## class, instance
-- are keywoard used to define typeclass, a way to guarantee that a type implements..
-- ..certain functions (or data).
-- *instance* allows a type to implement functions
-- *class* creates a new typeclass to implement through *instance*


{- IMPLEMENT A CLASSTYPE 1 INSTANCIATING 1-}
data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False
-- this is the same result as `deriving Eq` would give

data Date = Date DayOfWeek Int
  instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'
-- this is the same result as `deriving Eq` would give


{-- IMPLEMENT A CLASSTYPE 2 INSTANCIATING 2--}
-- functions when referred to type-classes are called methods.
-- > :info TypeClass
-- shows the methods of a class and then all the types that instanciates that class.
-- {-# MINIMAL method_that_must_be_implemented | or_other_method_must_be #-}
data Duo = Q Int Int
-- > Q 2 3
-- -> No instance for (Show Duo) arising from a use of ‘print’
instance Show Duo
-- instance TypeClassToImplement TypeForWhichWeWantToImplementThatTypeClass
-- -> No explicit implementation for either ‘showsPrec’ or ‘show’
-- notice that in :i Show {-# MINIMAL showsPrec | show #-}
data Duo = Q {q1 :: Int, q2 :: Int}
instance Show Duo where
  show q = "(" ++ show (q1 q) ++ ", " ++ show (q2 q) ++ ")"
-- > Q 2 3
-- ->(2, 3)
data Duo = Q {q1 :: Int, q2 :: Int} deriving Show
-- > Q 2 3
-- ->Q {q1 = 2, q2 = 3}
data Duo = Q Int Int deriving Show
-- > Q 2 3
-- ->Q 2 3

-- > :i Num
-- ->*
-- ->{-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
data Duo = D {d1 :: Int, d2 :: Int} deriving Show
instance Num Duo where
  duo0 + duo1 = D (d1 duo0 + d1 duo1) (d2 duo0 + d2 duo1)
  duo0 * duo1 = undefined 
  abs = undefined 
  signum = undefined 
  fromInteger = undefined
  negate = undefined
-- > D 2 3 + D 5 6
-- -> D {d1 = 7, d2 = 9}


{- IMPLEMENT A CLASSTYPE 3 INSTANCIATING 3 -}
{- W/ ARGUMENT -}
data Identity a = Identity a
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

--this is an alternative, even though it doesn't make sense why you'd use it
instance Ord a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = compare v v' == EQ


--another example to show how arguments can be in the data constructor but not 
-- ..type constructor
--it could be that you need type cnstr arg when you are using type variables (a, b..)
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

f :: TwoIntegers -> TwoIntegers -> Bool
f a b = a == b 
-- > f (Two 2 3) (Two 2 3)
-- -> True


{- WRAPPING TYPES with *newtype* -}
-- used to avoid creating types from someonelse's type which could create problems

data Duo = D {d1 :: Int, d2 :: Int} deriving Show

data PrettyDuo = PrettyDuo {accessorFunc :: Duo} --accessor is only useful to call teh wrapped type
instance Show PrettyDuo where
  show d = "(" ++ show (d1 d') ++ " " ++ show (d2 d') ++ ")" where
    d' = accessorFunc d
-- > PrettyDuo (D 23 12)
-- -> (23 12)

-- this way of wrapping gives your own type from another type you don't own
-- but this way create some runtime overhead as the code needs to unwrap the type first

-- to avoid runtime overhead you can use 'newtype':
newtype PrettyDuo = PrettyDuo {accessorFunc :: Duo} --accessor is only useful to call teh wrapped type
instance Show PrettyDuo where
  show d = "(" ++ show (d1 d') ++ " " ++ show (d2 d') ++ ")" where
    d' = accessorFunc d
-- this returns exactly the same as above




{- DEFINE A CLASS 1 -}
-- define our types
type Point = (Int, Int)
data Triangle = Triangle Point Point Point deriving (Show)
data Square = Square Point Point Point Point deriving (Show)
-- define typeclass
class Shape a where
  rotate :: a -> a
-- implement typeclass
instance Shape Triangle where 
  rotate (Triangle x y z) = Triangle z x y
instance Shape Square where
  rotate (Square x y z w) = Square w x y z
-- > t = Triangle (3,4) (4,5) (6,7) --notice using type for Point makes things easier here
-- > rotate t
-- -> Triangle (6,7) (3,4) (4,5)
-- > s = Square (3,4) (4,5) (6,7) (5,5)
-- > rotate s
-- -> Square (5,5) (3,4) (4,5) (6,7)

{- DEFINE A CLASS 2 -}
class Frobber a where
  frob :: a -> (String, Integer)

data A = A {valA :: Int} -- if not using the {val ::} you wouldn't have a way to get Int 
instance Frobber A where
  frob a = (show v, toInteger v) where v = valA a -- here we extract Int

data B = B {valB :: Double}
instance Frobber B where
  frob b = (show v, round v) where v = valB b

printTheFrob :: Frobber a => a -> IO ()
printTheFrob = print . frob

main :: IO ()
main = do
  printTheFrob (A 3)
  printTheFrob (B 4.5)




{- PATTERN MATCHING -}
{--}
data Colour = RGB | CMYK
colourModel :: Colour -> String 
colourModel RGB = "RGB"
colourModel CMYK = "CMYK"
{-/-}
{--}
data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Ord, Show)
calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y)
{-/-}
{--}
newHead :: [a] -> a
newHead [] = error "empty list"
newHead [a:xs] = a

newTail :: [a] -> a
newTail [] = error "empty list"
newTail [a:xs] = a
{-/-}
data WherePenguinsLive =
    Galapagos
  | Antartica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = 
  Peng WherePenguinsLive 
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool 
isSouthAfrica SouthAfrica = True 
isSouthAfrica _ = False 

gimmeWhereItLive :: Penguin -> WherePenguinsLive
gimmeWhereItLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool 
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antartica) = True 
antarcticPenguin _                = False 

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  galapagosPenguin p || antarcticPenguin p
{- // PATTERN MATCHING // -}


{- Case Of -}
data Colour = RGB | CMYK
colourModel :: Colour -> String 
colourModel val = 
  case val of
    RGB  -> "RGB"
    CMYK -> "CMYK"



{- LIST COMPRENSION -}
[ i * 2 | i <- [0..9] ]
-- -> [0,2,4,6,8,10,12,14,16,18]
div2 x = x `mod` 2 == 0
[ i * 2 | i <- [0..9], div2 i ]
-- -> [0,4,8,12,16]
{--}
--see the difference:
[ (row, col) | col <- [0..7], row <- [0..7] ]
[ [ (row, col) | col <- [0..7] ] | row <- [0..7] ]
--see how the '|' iterates the left side through the right side
--even nesting



{- FUNCTIONS -}
zip [1..3] ['a'..'z']
-- ->[(1,'a'),(2,'b'),(3,'c')]
{- zip, zipWith -}
zip [[0,0,0],[1,1,1],[2,2,2]] [[0,1,2],[0,1,2],[0,1,2]]
--[([0,0,0],[0,1,2])
--,([1,1,1],[0,1,2])
--,([2,2,2],[0,1,2])]
zipWith (++) [[0,0,0],[1,1,1],[2,2,2]] [[0,1,2],[0,1,2],[0,1,2]]
--[[0,0,0,0,1,2]
--,[1,1,1,0,1,2]
--,[2,2,2,0,1,2]]
zipWith zip [[0,0,0],[1,1,1],[2,2,2]] [[0,1,2],[0,1,2],[0,1,2]]
--[[(0,0),(0,1),(0,2)]
--,[(1,0),(1,1),(1,2)]
--,[(2,0),(2,1),(2,2)]]
```

# [Functor](https://mmhaskell.com/monads/functors)

A functor is a mapping. said in a longer fashion:

> A Functor allows us to transform the underlying values with a function, so that the values are all updated, but the structure of the container is the same

Said in a computer science way: A Functor is any type that can act as a generic container.

Haskell represents the concept of a functor with the Functor typeclass. This typeclass has a single required function `fmap`.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

We can see that `fmap` is a higher ordered function taking two inputs.

  1. A transformation function from an a type to a b type (a and b can be the same if we want)
  2. A functor containing values of type a

The output it produces is a new functor containing values of type b. This new functor has exactly the same structure (or shape) as the input functors; all that has changed is that each element has been modified by the input function.

So is important to understand that `f` isn't a function, but is a type that takes a type parameter.

And that should make you understand that `f` is just a container type that contains a cetains type, that is a parameter therefor generic. Generic Container.

You can make a Functor instance for any parameterized type that can act as a container. The only thing you really need to fill in is the fmap implementation. It will just look like this:

```haskell
instance Functor MyType where
  fmap f (MyType ...) = ...
```

# [Applicative](https://mmhaskell.com/monads/applicatives)

like functor but instead of only one function, you actually have a container of unctions too, that gets applied to all elements.

```haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

```haskell
>> [(+2), (*3)] <*> [4, 6]
[6, 8, 12, 18]
```

```haskell
>> pure (+) <*> [1, 2, 3] <*> [4, 5, 6]
[5, 6, 7, 6, 7, 8, 7, 8, 9]
>> (+) <$> [1, 2, 3] <*> [4, 5, 6]
[5, 6, 7, 6, 7, 8, 7, 8, 9]
```

Notice that `<$>` is the same as `fmap` the functor function.


```haskell
>> let f a b c = a + b + c
>> pure f <*> [1, 2, 3] <*> [10, 15] <*> [100, 200]
[111, 211, 116, 216, 112, 212, 117, 217, 113, 213, 118, 218]
```


# Array `Data.Array`

`Array i e` is a type. It's defined in a module which does not export its data constructors, so we don't get access to those. This is unlike the `Maybe a` type for which we get access to the `Nothing` and `Just` constructors.

Hence, to create an array value we need to rely on the functions exported by the module. There is indeed a function for that, named array. This function is sometimes referred to as a "smart" constructor, since unlike a (regular) constructor it performs some sanity checks before constructing the value.

Anyway, it is meant to be used like this:

```haskell
array (startPos, endPos) [ (startPos, value0), (nextPos, value1), ..., (endPos, valueLast) ]
```

if you board only contains position (0,0) (should it be so?) then it should be created as

```haskell
array ((0,0),(0,0)) [ ((0,0), Just (Piece White Pawn)) ]
```

If instead you need to create a 8x8 board, you will need a longer list for that.

```haskell
array ((0,0),(7,7)) [ ((0,0), Just (Piece White Pawn)) , ....
```

If you need to place only a few pieces in a large board, it might be more convenient to simply create an empty board and then update it with the few pieces. E.g.

```haskell
let emptyB = array ((0,0),(7,7)) [ ((x,y), Nothing) | x<-[0..7], y<-[0..7] ]
    myB = emptyB // [ ((5,5), Just (Piece White Pawn)) ]
in ...
```

Here I used the `//` operator to create a new board with the same pieces as emptyB, except for those positions and values in the list.

In `chesshs` ibrary teh Board is defined as 

```haksell
data Board = Board { turn :: Color
                   , castlingAvail :: String
                   , enpassant :: Maybe (Int, Int)
                   , board :: Array (Int, Int) (Maybe Piece)
                   } deriving (Eq)
```

is to notice that here is the Array type that is defined. But you won't create such Array with the Array consructor becasue it is not revealed. Instead they will let you create the Array of type `(Int, Int) (Maybe Piece)` through the method `array` which has a different type signature.

so Array has a type of

```haskell
type Array :: * -> * -> *
data Array i e
```

There isn't only one concrete type, because this type is parametrized, requiring two concrete type before having the third concrete type as output (Array).

And that's what the board argument is showing, that `i e` as `(Int, Int) (Maybe Piece)`.

The `array` method is what creates that Array.

```haskell
array :: (i, i) -> [(i, e)] -> Array i e
```

which is exactly what we used to create the Board above.