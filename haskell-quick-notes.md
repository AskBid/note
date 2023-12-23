# Index

[List Comprehension](#list-comprehension) &nbsp;&nbsp;&nbsp;&nbsp;  `[ x | x <- [1..5], x < 4 ]`

## Syntax in Functions

[Pattern Mattching](#pattern-matching) &nbsp;&nbsp;&nbsp;&nbsp;  `head' (x:_) = x`

[As Patterns](#as-patterns) &nbsp;&nbsp;&nbsp;&nbsp;  `capital all@(x:xs)= "first letter of " ++ all ++ " is " ++ [x]`

[Guards](#guards) &nbsp;&nbsp;&nbsp;&nbsp;  `| bmi <= 18.5 = "underweight"`

[Where](#where) &nbsp;&nbsp;&nbsp;&nbsp;  `where bmi = weight / height ^ 2`

[Let in](#let-it-be) &nbsp;&nbsp;&nbsp;&nbsp;  `let square x = x * x in (square 5, square 3, square 2)`

[Case Expressions](#case-expressions) &nbsp;&nbsp;&nbsp;&nbsp;  `head' xs = case xs of [] -> head' xs = case xs of (x:_) -> x`

## Recursion

[Recursion](#recursion) &nbsp;&nbsp;&nbsp;&nbsp;  `reverse' (x:xs) = reverse' xs ++ [x]`

## Higher Order Functions

[Curied Functions](#curied-functions)

## Making Our Own Types and Typeclasses

[Recusive Data Structure](#recusive-data-structure)

[Typeclasses](#typeclasses) &nbsp;&nbsp;&nbsp;&nbsp;  `class Eq a where`

[YesNo Typeclass Instantiate Example](#yesno-typeclass-instantiate-example) &nbsp;&nbsp;&nbsp;&nbsp; `instance YesNo Int where`

[The Functor typeclass](#the-functor-typeclass)  &nbsp;&nbsp;&nbsp;&nbsp;  `instance Functor Maybe where; fmap f (Just x) = Just (f x)`

[Two Type Parameters Functor](#two-type-parameters-functor) &nbsp;&nbsp;&nbsp;&nbsp;  `(b -> c) -> (Either a) b -> (Either a) c`

## Monad

[Intro to Monads](#monad) &nbsp;&nbsp;&nbsp;&nbsp;  `do k2 <- lookup k1 al ; return ("You " ++ k2)`

<br>

***

<br>

## list Comprehension
[index](#index)

```haskell
ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  
ghci> rightTriangles'  
[(6,8,10)]  
```

With imperative programming, we would have solved it by nesting three loops and then testing if the current combination satisfies a right triangle and if it has the right perimeter. If that's the case, we would have printed it out to the screen or something. In functional programming, that pattern is achieved with mapping and filtering. You make a function that takes a value and produces some result. We map that function over a list of values and then we filter the resulting list out for the results that satisfy our search. Thanks to Haskell's laziness, even if you map something over a list several times and filter it several times, it will only pass over the list once.

# Syntax in Functions

## [Pattern Matching](http://learnyouahaskell.com/syntax-in-functions#pattern-matching)
[index](#index)

```haskell
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x
```

```haskell
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
```

### [As Patterns](https://stackoverflow.com/questions/1153465/what-does-the-symbol-mean-in-reference-to-lists-in-haskell)
[index](#index)

```haskell
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  
```

## [Guards](http://learnyouahaskell.com/syntax-in-functions#guards-guards)
[index](#index)

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"
```

## [Where](http://learnyouahaskell.com/syntax-in-functions#where)
[index](#index)

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2 
```

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
```

```haskell
...  
where bmi = weight / height ^ 2  
      (skinny, normal, fat) = (18.5, 25.0, 30.0)  
```

```haskell
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  
```

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
```

## [Let it Be](http://learnyouahaskell.com/syntax-in-functions#let-it-be)
[index](#index)

Very similar to `where` bindings.

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 
```

```haskell
ghci> 4 * (let a = 9 in a + 1) + 2  
42  
```

```haskell
ghci> [let square x = x * x in (square 5, square 3, square 2)]  
[(25,9,4)] 
```

We omitted the in part of the let binding when we used them in list comprehensions because the visibility of the names is already predefined there.

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]  
```

The in part can also be omitted when defining functions and constants directly in GHCi. If we do that, then the names will be visible throughout the entire interactive session.

```haskell
ghci> let zoot x y z = x * y + z  
ghci> zoot 3 9 2  
29  
ghci> let boot x y z = x * y + z in boot 3 4 2  
14  
ghci> boot  
<interactive>:1:0: Not in scope: `boot'  
```

If let bindings are so cool, why not use them all the time instead of where bindings, you ask? Well, since let bindings are expressions and are fairly local in their scope, they can't be used across guards. Some people prefer where bindings because the names come after the function they're being used in. That way, the function body is closer to its name and type declaration and to some that's more readable.

## Case Expressions
[index](#index)

Pattern matching is actually just syntactic sugar for case expressions. These two pieces of code do the same thing and are interchangeable:

```haskell
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  
```

```haskell
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  
```

As you can see, the syntax for case expressions is pretty simple:

```haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
```

# [Recursion](http://learnyouahaskell.com/recursion)
[index](#index)

Usually you define an edge case and then you define a function that does something between some element and the function applied to the rest.


```haskell
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  
```

First we set up an edge condition and say that the maximum of a singleton list is equal to the only element in it. Then we say that the maximum of a longer list is the head, if the head is bigger than the maximum of the tail. If the maximum of the tail is bigger, well, then it's the maximum of the tail.

```haskell
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []                       -- edge condition:
    | otherwise = x:replicate' (n-1) x     -- at some point is always reached.
```

```haskell
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  
```

The guard is used as an if here, without otherwise if `n > 0` the matching will fall through the next pattern.

```haskell
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  
```

```haskell
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 
```

```haskell
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs   
```

```haskell
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
```

```haskell
Prelude> xs = [1,34,56,53,3]
Prelude> x = 55
Prelude> [a | a <- xs, a <= x]
[1,34,53,3]
```

# Higher Order Functions

Haskell functions can take functions as parameters and return functions as return values. A function that does either of those is called a higher order function.

## [Curied Functions](http://learnyouahaskell.com/higher-order-functions#curried-functions)
[index](#index)

Every function in Haskell officially only takes one parameter.

```haskell
ghci> max 4 5
5
ghci> (max 4) 5
5
```

Let's examine the type of max. It's `max :: (Ord a) => a -> a -> a`. That can also be written as `max :: (Ord a) => a -> (a -> a)`. That could be read as: `max` takes an `a` and returns (that's the `->`) a function that takes an `a` and returns an `a`.
functions take several parameters despite each function actually taking only one parameter and returning partially applied functions until we reach a function that returns a solid value

```haskell
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  
```

```haskell
ghci> let multTwoWithNine = multThree 9  
ghci> multTwoWithNine 2 3  
54  
ghci> let multWithEighteen = multTwoWithNine 2  
ghci> multWithEighteen 10  
180 
```

By calling functions with too few parameters, so to speak, we're creating new functions on the fly.

```haskell
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x 
```

Is same as 

```haskell
compareWithHundred = compare 100 
```

Infix functions can also be partially applied by using sections.

```haskell
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  
```

## [Higher Order Programming](http://learnyouahaskell.com/higher-order-functions#higher-orderism)
[index](#index)

```haskell
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)
```

```haskell
ghci> applyTwice (+3) 10  
16  
ghci> applyTwice (++ " HAHA") "HEY"  
"HEY HAHA HAHA"  
ghci> applyTwice ("HAHA " ++) "HEY"  
"HAHA HAHA HEY"  
ghci> applyTwice (multThree 2 2) 9  
144  
ghci> applyTwice (3:) [1]  
[3,3,1] 
```

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  
```

`a ,b ,c` don't have to be of the same type, but they can. If the type declaration of a function says it accepts an `a -> b -> c` function as a parameter, it will also accept an `a -> a -> a` function, but not the other way around.

```haskell
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]  
[6,8,7,9]  
ghci> zipWith' max [6,3,2,1] [7,3,1,5]  
[7,3,2,5]  
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]  
["foo fighters","bar hoppers","baz aldrin"]  
ghci> zipWith' (*) (replicate 5 2) [1..]  
[2,4,6,8,10]  
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
[[3,4,6],[9,20,30],[10,12,12]]  
```

```haskell
flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x 
```

because functions are curried by default, the second pair of parentheses is really unnecessary, because -> is right associative by default. `(a -> b -> c) -> (b -> a -> c)` is the same as `(a -> b -> c) -> (b -> (a -> c))`, which is the same as `(a -> b -> c) -> b -> a -> c`. We wrote that `g x y = f y x`. If that's true, then `f y x = g x y` must also hold. Keeping that in mind, we can define this function in an even simpler manner.

```haskell
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y  
```

When we call `flip' f` without the parameters `y` and `x`, it will return an `f` that takes those two parameters but calls them flipped.

```haskell
ghci> flip' zip [1,2,3,4,5] "hello"  
[('h',1),('e',2),('l',3),('l',4),('o',5)]  
ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]  
[5,4,3,2,1]  
```

## [Maps and filters](http://learnyouahaskell.com/higher-order-functions#maps-and-filters)
[index](#index)

```haskell
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs 
```

```haskell
ghci> map (+3) [1,5,3,1,6]  
[4,8,6,4,9]  
ghci> map (++ "!") ["BIFF", "BANG", "POW"]  
["BIFF!","BANG!","POW!"]  
ghci> map (replicate 3) [3..6]  
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]  
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
[[1,4],[9,16,25,36],[49,64]]  
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  
[1,3,6,2,2] 
```

Each of these could be achieved with a list comprehension. `map (+3) [1,5,3,1,6]` is the same as writing `[x+3 | x <- [1,5,3,1,6]]`. `map` is much more readable for cases where you only apply some function to the elements of a list, especially once you're dealing with maps of maps and then the whole thing with a lot of brackets can get a bit messy.

```haskell
filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs 
```

```haskell
ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]  
[[1,2,3],[3,4,5],[2,2]]  
ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  
"uagameasadifeent"  
ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"  
"GAYBALLS"  
```

All of this could also be achived with list comprehensions by the use of predicates. There's no set rule for when to use map and filter versus using list comprehension, you just have to decide what's more readable depending on the code and the context.

```haskell
--with list comprehension.
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

--with filter is more readable.
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted 
```

Find the **largest** number under 100,000 that's divisible by 3829.

```haskell
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0 
```

That's laziness in action again. Because we only end up using the head of the filtered list, it doesn't matter if the filtered list is finite or infinite. The evaluation stops when the first adequate solution is found.

### `takeWhile` 
[index](#index)

If we wanted to get the first word of the string `"elephants know how to party"`, we could do `takeWhile (/=' ') "elephants know how to party"` and it would return `"elephants"`.

```haskell
Prelude> :t (==' ')
(==' ') :: Char -> Bool
Prelude> :t (/=' ')
(/=' ') :: Char -> Bool
Prelude> (/=' ') 'c'
True
Prelude> (/=' ') ' '
False
```

The sum of all odd squares that are smaller than 10,000.

```haskell
ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  
166650 
ghci> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  
166650  
```

What type does this return? `map (*2) [1..]`

```haskell
(Num a) => [a]
```

What type does this return? `map (*) [1..]`
```haskell
(Num a) => [a => a]
```

```haskell
ghci> let listOfFuns = map (*) [0..]  
ghci> (listOfFuns !! 4) 5  
20  
```

`!!` takes is of type `[a] => Int => a` and takes an array and an integer returning the element with the index of the Int.

## [Lambdas](http://learnyouahaskell.com/higher-order-functions#lambdas)
[index](#index)

`(\parameters -> functionBody)`

```haskell
numLongChains :: Int  
numLongChains = length (filter isLong [[1..5],[1..34],[5..30]])  
    where isLong xs = length xs > 15 
```

Thanks to Lambdas can be shortened to

```haskell
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) [[1..5],[1..34],[5..30]]) 
```

People who are not well acquainted with how currying often use lambdas where they don't need to. For instance, the expressions `map (+3) [1,6,3,2]` and `map (\x -> x + 3) [1,6,3,2]` are equivalent since both `(+3)` and `(\x -> x + 3)` are functions that take a number and add 3 to it. Needless to say, making a lambda in this case is stupid since using partial application is much more readable.

Like normal functions, lambdas can take any number of parameters.
Lambdas are normally surrounded by parentheses unless we mean for them to extend all the way to the right.

```haskell
--few examples

ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
[153.0,61.5,31.0,15.75,6.6]

ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  
[3,8,9,8,7]
```

A gimmick to illustrate currying:

```haskell
addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z  

--is same as

addThree :: (Num a) => a -> a -> a -> a  
addThree = \x -> \y -> \z -> x + y + z 
```

However can be useful to make th e flip' function from earlier more readable:

```haskell
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f = \x y -> f y x 
```

We make it obvious that this will be used for producing a new function most of the time. The most common use case with flip is calling it with just the function parameter and then passing the resulting function on to a map or a filter. So use lambdas in this way when you want to make it explicit that your function is mainly meant to be partially applied and passed on to a function as a parameter.

## [Folds](http://learnyouahaskell.com/higher-order-functions#folds)

### Fold Left function `foldl`
[index](#index)

It will fold the list from the left side.

A fold takes a binary function, a starting value (I like to call it the accumulator) and a list to fold up. The binary function itself takes two parameters. Its point is to reduce the list to an accumulator value.

```haskell
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 
```

```haskell
ghci> sum' [3,5,2,1]  
11  
```

Taking currying into account we can write `sum'` more succintly as

```haskell
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0 
```

The lambda function `(\acc x -> acc + x)` is the same as `(+)`. We can omit the `xs` as the parameter because calling `foldl (+) 0` will return a function that takes a list. Generally, if you have a function like **`foo a = bar b a`**, you can rewrite it as **`foo = bar b`**, because of currying.

```haskell
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
```

`elem` checks weather a value is part of a list.

### Fold Right function `foldr`
[index](#index)

The left fold's binary function has the accumulator as the first parameter and the current value as the second one `(\acc x -> ...)`, the right fold's binary function has the current value as the first parameter and the accumulator as the second one `(\x acc -> ...)`.

```haskell
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs
```

The accumulator value (and hence, the result) of a fold can be of any type. It can be a number, a boolean or even a new list.

Why not to use `foldl` as `map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs`? because `++` function is much more expensive than `:`, so we usually use `foldr` when we're building up new lists from a list.

`foldr` work on infinite lists, whereas `foldl` don't. That is if you pick a point in the list, `foldr` will reach the beginning of hte list at some point while `foldl` will never reach the end.

**Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that. Whenever you want to traverse a list to return something, chances are you want a fold.** That's why folds are, along with maps and filters, one of the most useful types of functions in functional programming.

### `foldl1` and `foldl1`
[index](#index)

Work much like `foldl` and `foldr`, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.

Because they depend on the lists they fold up having at least one element, they cause runtime errors if called with empty lists. `foldl` and `foldr`, on the other hand, work fine with empty lists. When making a fold, think about how it acts on an empty list. If the function doesn't make sense when given an empty list, you can probably use a `foldl1` or `foldr1` to implement it.

```haskell
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)
```

### `scanl` and `scanr`
[index](#index)

`scanl` and `scanr` are like `foldl` and `foldr`, only they report all the intermediate accumulator states in the form of a list. There are also `scanl1` and `scanr1`, which are analogous to `foldl1` and `foldr1`.

```haskell
ghci> scanl (+) 0 [3,5,2,1]  
[0,3,8,10,11]  
ghci> scanr (+) 0 [3,5,2,1]  
[11,8,3,1,0]  
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
[3,4,5,5,7,9,9,9]  
ghci> scanl (flip (:)) [] [3,2,1]  
[[],[3],[2,3],[1,2,3]] 
```

```haskell
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1 
```

```haskell
ghci> sqrtSums  
131  
ghci> sum (map sqrt [1..131])  
1005.0942035344083  
ghci> sum (map sqrt [1..130])  
993.6486803921487 
```

## [$](http://learnyouahaskell.com/higher-order-functions#function-application)
[index](#index)

When a `$` is encountered, the expression on its right is applied as the parameter to the function on its left. `$` has the lowest precedence of any operator.

`sum (map sqrt [1..130])` is same as `sum $ map sqrt [1..130]`
`sqrt (3 + 4 + 9)` is same as `sqrt $ 3 + 4 + 9`
`f (g (z x))` is equal to `f $ g $ z x`
`sum (filter (> 10) (map (*2) [2..10]))` as `sum $ filter (> 10) $ map (*2) [2..10]`

But apart from getting rid of parentheses, `$` means that function application (that's just another name for `$`) can be treated just like another function. That way, we can, for instance, map function application over a list of functions.

```haskell
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]  
[7.0,30.0,9.0,1.7320508075688772] 
```

`$` definition:

```haskell
($) :: (a -> b) -> a -> b  
f $ x = f x 
```

## [Function Composition (.)](http://learnyouahaskell.com/higher-order-functions#composition)
[index](#index)

`f . g x = f (g x)` composing two functions `f` and `g` produces a new function that is equivalent to calling `g` and then calling `f` with the result of `g`.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  
```

The expression `negate . (* 3)` returns a function that takes a number, multiplies it by 3 and then negates it.

One of the uses is making function on the fly as it is cleaner than lambdas.

```haskell
ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24]  

ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24] 
```

Function composition is right-associative, so we can compose many functions at a time. The expression `f (g (z x))` is equivalent to `(f . g . z) x`. With that in mind, we can turn

```haskell
ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
[-14,-15,-27]  

ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
[-14,-15,-27]
```

What about having several parameters?

```haskell
sum (replicate 5 (max 6.7 8.9)) 
--can be rewritten as 
(sum . replicate 5 . max 6.7) 8.9 
--or as 
sum . replicate 5 . max 6.7 $ 8.9
```

A function that takes what `max 6.7` takes and applies `replicate 5` to it is created. Then, a function that takes the result of that and does a `sum` of it is created. Finally, that function is called with `8.9`. Basically `$` is like an **hold on a sec**.

Basically if you have a lot of parentesis you can just put last parameter after `$` and points between all the other functions.

```haskell
replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
--you can write it as 
replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
```

### Point Free Style
[index](#index)

It means that it doesn't have any explicit arguments. They are tacit arguments.

```haskell
sum' :: (Num a) => [a] -> a     
sum' xs = foldl (+) 0 xs 
```

When you have a parameter on both side of the pattern (`xs`), you can rewrite it without it (thanks to currying).

```haskell
sum' :: (Num a) => [a] -> a     
sum' = foldl (+) 0
```

But what to do when the parameter si inside parenthesis?

```haskell
fn x = ceiling (negate (tan (cos (max 50 x))))  
```

### Function Composition 
[index](#index)

Function Composition can come in help.

```haskell
fn = ceiling . negate . tan . cos . max 50  
```

For readability style is better to use bindings to give labels to intermediary results or split the problem into sub-problems and then put it together so that the function makes sense to someone reading it instead of just making a huge composition chain.

```haskell
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..] 

--but in relaity is more readable as
oddSquareSum :: Integer  
oddSquareSum =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  
```

# [Modules](http://learnyouahaskell.com/modules)

## Loading Modules
[index](#index)

The syntax for importing modules in a Haskell script is `import <module name>`

```haskell
import Data.List  
  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub
```

With `import Data.List`, all the functions that `Data.List` exports become available in the global namespace. Like `nub`.
`length . nub` produces a function that's the equivalent of `\xs -> length (nub xs)`.

To load in GHCI

```haskell
ghci> :m + Data.List  
```

```haskell
ghci> :m + Data.List Data.Map Data.Set 
```

To import only selected funs from a module

```haskell
import Data.List (nub, sort)  
```

To import everything but selected funs from a mod

```haskell
import Data.List hiding (nub)  
```

When the imported mod has funs named the same as exisiting funs (in the prelude mod)

```haskell
import qualified Data.Map  
```

This makes it so that if we want to reference `Data.Map`'s `filter` function, we have to do `Data.Map.filter`, whereas just `filter` still refers to the normal `filter` we all know from the automatically loaded prelude module. 

```haskell
import qualified Data.Map as M  
```

Now, to reference `Data.Map.filter` function, we just use `M.filter`.

Use [this handy reference]() to see which modules are in the standard library. A great way to pick up new Haskell knowledge is to just click through the standard library reference and explore the modules and their functions. You can also view the Haskell source code for each module. Reading the source code of some modules is a really good way to learn Haskell and get a solid feel for it.

## [Data.List](http://learnyouahaskell.com/modules#data-list)
[source](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.17.0.0/Data-List.html)

**`map`**
**`filter`**
are from this module, check the link to see other funs.

**`\\`** is the list difference function. 

```haskell
ghci> [1..10] \\ [2,5,9]  
[1,3,4,6,7,8,10]  
ghci> "Im a big baby" \\ "big"  
"Im a  baby"
```

Doing `[1..10] \\ [2,5,9]` is like doing `delete 2 . delete 5 . delete 9 $ [1..10]`.

## [Data.Char](http://learnyouahaskell.com/modules#data-char)
[source](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.17.0.0/Data-Char.html)
[index](#index)

Functions that deal with characters.

```haskell
ghci> ord 'a'  
97  
ghci> chr 97  
'a'  
ghci> map ord "abcdefgh"  
[97,98,99,100,101,102,103,104] 
```

```haskell
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted 
```

Simple encoder to shift character, Caesar Cipher. The above could also be writte from a composition cowboy as `map (chr . (+ shift) . ord) msg`

```haskell
ghci> encode 3 "Heeeeey"  
"Khhhhh|"  
ghci> encode 4 "Heeeeey"  
"Liiiii}"  
ghci> encode 1 "abcd"  
"bcde"  
ghci> encode 5 "Marry Christmas! Ho ho ho!"  
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
```

```haskell
decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg  
```

```haskell
ghci> encode 3 "Im a little teapot"  
"Lp#d#olwwoh#whdsrw"  
ghci> decode 3 "Lp#d#olwwoh#whdsrw"  
"Im a little teapot"  
ghci> decode 5 . encode 5 $ "This is a sentence"  
"This is a sentence"
```

## [Data.Map](http://learnyouahaskell.com/modules#data-map)
[index](#index)

Association lists (also called dictionaries or **maps**) are lists that are used to store key-value pairs where ordering doesn't matter.

```haskell
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ] 
```

Looking up some value by key.

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> v  
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
```

### `Maybe`
[index](#index)

But what happens if the key we're looking for isn't in the association list?
We'll end up trying to get the head of an empty list, which throws a runtime error.
To avoid the program to crash let's use the `Maybe` data type. If we don't find the key, we'll return a `Nothing`. If we find it, we'll return `Just something`, where something is the value corresponding to that key.

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key [] = Nothing  
findKey key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey key xs
```

if you look the code above is the classic `fold` pattern, so let's see how this would be implemented as a `fold`.

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
```

```haskell
ghci> findKey "betty" phoneBook  
Just "555-2938"  
ghci> findKey "wilma" phoneBook  
Nothing 
```

We just implemented the `lookup` function from `Data.List`. If we want to find the corresponding value to a key, we have to traverse all the elements of the list until we find it. The `Data.Map` module offers association lists that are much faster (because they're internally implemented with trees) and also it provides a lot of utility functions.

Because `Data.Map` exports functions that clash with the `Prelude` and `Data.List` ones, we'll do a qualified `import`.

```haskell
import qualified Data.Map as Map
```

notice the difference:

```haskell
*Main> :t Map.map
Map.map :: (a -> b) -> Map.Map k a -> Map.Map k b
*Main> :t map
map :: (a -> b) -> [a] -> [b]
*Main> 
```

Here some funs of this module

`Map.fromList`

```haskell
ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]  
fromList [(1,2),(3,2),(5,5)]
```

```haskell
Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v  
```

Takes a map in the form of a list and returns a map with same association removing the duplicates if there were any.

In our version of map as lists, the keys only had to be equatable (their type belonging to the `Eq` typeclass) but now they have to be orderable (`Ord`). That's an essential constraint in the `Data.Map` module. It needs the keys to be orderable so it can arrange them in a tree.

`Map.empty` represents an empty map.

```haskell
ghci> Map.empty
fromList [] 
```

`insert` takes a key, a value and a map and returns a new map that's just like the old one, only with the key and value inserted.

```haskell
ghci> Map.empty  
fromList []  
ghci> Map.insert 3 100 Map.empty  
fromList [(3,100)]  
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))  
fromList [(3,100),(4,200),(5,600)]  
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty  
fromList [(3,100),(4,200),(5,600)] 
```

We can implement our own `fromList` by using the empty map, insert and a fold. Watch:

```haskell
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
```

`null` checks if a map is empty.

```haskell
ghci> Map.null Map.empty  
True  
ghci> Map.null $ Map.fromList [(2,3),(5,5)]  
False  
```

`size` reports the size of a map.

```haskell
ghci> Map.size Map.empty  
0  
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]  
5  
```

`singleton` takes a key and a value and creates a map that has exactly one mapping.

```haskell
ghci> Map.singleton 3 9  
fromList [(3,9)]  
ghci> Map.insert 5 9 $ Map.singleton 3 9  
fromList [(3,9),(5,9)]  
```

`lookup` works like the `Data.List` `lookup`, only it operates on maps therefore acts quicker because maps are ordered. It returns `Just` something if it finds something for the key and `Nothing` if it doesn't.

`member` is a predicate takes a key and a map and reports whether the key is in the map or not.

```haskell
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]  
True  
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]  
False  
```

`map` and `filter` work much like their list equivalents.

```haskell
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]  
fromList [(1,100),(2,400),(3,900)]  
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]  
fromList [(2,'A'),(4,'B')]
```

`toList` is the inverse of `fromList`.

```haskell
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3  
[(4,3),(9,2)]
```

`keys` and `elems` return lists of keys and values respectively. `keys` is the equivalent of `map fst . Map.toList` and `elems` is the equivalent of `map snd . Map.toList`.

`fromListWith` is a cool little function. It acts like `fromList`, only it doesn't discard duplicate keys but it uses a function supplied to it to decide what to do with them.

```haskell
*Main> :t Map.fromListWith
Map.fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map.Map k a
```

```haskell
phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]  
```

```haskell
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
"939-8282"
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
"342-2492, 555-2938"
```

Alternatively:

```haskell
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook  
["827-9162","943-2929","493-2928"]
```

Another use case of `fromListWith`

```haskell
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
fromList [(2,100),(3,29),(4,22)]
```

```haskell
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
fromList [(2,108),(3,62),(4,37)]
```

`insertWith` is to `insert` what `fromListWith` is to `fromList`. It inserts a key-value pair into a map, but if that map already contains the key, it uses the function passed to it to determine what to do.

```haskell
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]  
fromList [(3,104),(5,103),(6,339)] 
```

## [Data.Set](http://learnyouahaskell.com/modules#data-set)
[Source](https://downloads.haskell.org/ghc/latest/docs/libraries/containers-0.6.6/Data-Set.html)
[index](#index)

The `Data.Set` module offers like sets from mathematics. Sets are kind of like a cross between lists and maps. All the elements in a set are unique. And because they're internally implemented with trees (much like maps in `Data.Map`), they're **ordered**. Checking for membership, inserting, deleting, etc. is **much faster** than doing the same thing with lists.

Because the names in `Data.Set` clash with a lot of `Prelude` and `Data.List` names, we do a qualified import.

```haskell
import qualified Data.Set as Set
```

`fromList` function works much like you would expect. It takes a list and converts it into a set.

```haskell
ghci> text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
ghci> text2 = "The old man left his garbage can out and now his trash is all over my lawn!"  
ghci> let set1 = Set.fromList text1  
ghci> let set2 = Set.fromList text2  
ghci> set1  
fromList " .?AIRadefhijlmnorstuy"  
ghci> set2  
fromList " !Tabcdefghilmnorstuvwy"
```

`intersection`

```haskell
ghci> Set.intersection set1 set2  
fromList " adefhilmnorstuy"
```

`difference`

```haskell
ghci> Set.difference set1 set2  
fromList ".?AIRj"  
ghci> Set.difference set2 set1  
fromList "!Tbcgvw" 
```

`union`

```haskell
ghci> Set.union set1 set2  
fromList " !.?AIRTabcdefghijlmnorstuvwy"
```

More funs:

```haskell
ghci> Set.null Set.empty  
True  
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]  
False  
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]  
3  
ghci> Set.singleton 9  
fromList [9]  
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]  
fromList [1,3,4,8,9]  
ghci> Set.insert 8 $ Set.fromList [5..10]  
fromList [5,6,7,8,9,10]  
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]  
fromList [3,5]  
ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]  
False  
ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
False
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,5,7]  
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,4,5,6,7,8]
```

Sets are often used to weed a list of duplicates from a list by first making it into a set with `fromList` and then converting it back to a list with `toList`. The `Data.List` function `nub` already does that, but weeding out duplicates for large lists is much faster with sets. But using `nub` only requires `Eq` typeclass, whereas if you want to cram elements into a set the type of the list has to be `Ord`.

```haskell
ghci> let setNub xs = Set.toList $ Set.fromList xs  
ghci> setNub "HEY WHATS CRACKALACKIN"  
" ACEHIKLNRSTWY"  
ghci> nub "HEY WHATS CRACKALACKIN"  
"HEY WATSCRKLIN"  
```
`setNub` is generally faster than `nub` on big lists but `nub` preserves the ordering of the elements, while `setNub` does not.

## [Making our own modules](http://learnyouahaskell.com/modules#making-our-own-modules)
[index](#index)

Create a file called `Geometry.hs`.
We say that a module *exports* functions. What that means is that when I import a module, I can use the functions that it exports. It can define functions that its functions call internally, but we can only see and use the ones that it exports.

At the beginning of a module, we specify the module name. If we have a file called `Geometry.hs`, then we should name our module `Geometry`. Then, we specify the functions that it exports and after that, we can start writing the functions.

```haskell
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  
  
sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  
  
cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b 
```

Notice how `rectangleArea` is internal and not exported.

To use our module, we just do:

```haskell
import Geometry 
```

`Geometry.hs` has to be in the same folder that the program that's importing it is in, though.

### Submodules
[index](#index)

Modules can also be given a hierarchical structures. Each module can have a number of sub-modules and they can have sub-modules of their own. Let's section the functions in Geometry in a module that has three sub-modules, one for each type of object.

Make a folder called `Geometry`. Mind the capital G. In it, we'll place three files: `Sphere.hs`, `Cube.hs`. Here's what the files will contain.

```haskell
module Geometry.Sphere  
( volume  
, area  
) where  
  
volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2) 
```

```haskell
module Geometry.Cube  
( volume  
, area  
) where  
  
import qualified Geometry.Cuboid as Cuboid  
  
volume :: Float -> Float  
volume side = Cuboid.volume side side side  
  
area :: Float -> Float  
area side = Cuboid.area side side side
```

Notice how we placed it in a folder called `Geometry` and then defined the module name as `Geometry.Sphere`.

```haskell
import Geometry.Sphere
```

If you import more than one module, it is likely that you'll have conflicting names, therefore qualified imports will be used.

```haskell
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cube as Cube 
```

# [Making Our Own Types and Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)
[index](#index)

```haskell
data Bool = False | True  
```

`data` means that we're defining a new data type. The part before the `=` denotes the **type**, which is `Bool`. The parts after the `=` are **value constructors**. They specify the different values that this type can have. The `|` is read as or. So we can read this as: the `Bool` type can have a value of `True` or `False`. Both the **type name** and the **value constructors** have to be capital cased.

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

As seen here above when we write a value constructor, we can optionally add some types after it and those types define the values it will contain.

**Value constructors are actually functions that ultimately return a value of a data type**.
Let's take a look at the type signatures for these two value constructors.

```haskell
ghci> :t Circle  
Circle :: Float -> Float -> Float -> Shape  
ghci> :t Rectangle  
Rectangle :: Float -> Float -> Float -> Float -> Shape  
```

So value cnstructuros are functions exactly like everything else.

```haskell
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

It says that the function takes a `Shape` and returns a `Float`. We couldn't write a type declaration of `Circle -> Float` because **`Circle` is not a type, `Shape` is**. Just like we can't write a function with a type declaration of `True -> Int`.

```haskell
ghci> surface $ Circle 10 20 10  
314.15927  
ghci> surface $ Rectangle 0 0 100 100  
10000.0  
```

But if we try to just print out `Circle 10 20 5` in the prompt, we'll get an error. That's because Haskell doesn't know how to display our data type as a string. Remember, when we try to print a value out in the prompt, Haskell first runs the `show` function to get the string representation of our value and then it prints that out to the terminal. To make our `Shape` type part of the `Show` typeclass, we modify it like this:

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)  
```

```haskell
ghci> Circle 10 20 5  
Circle 10.0 20.0 5.0  
ghci> Rectangle 50 230 60 90  
Rectangle 50.0 230.0 60.0 90.0  
```

Value constructors are functions, so we can map them and partially apply them and everything.

```haskell
ghci> map (Circle 10 20) [4,5,6,6]  
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
```

Let's refine our data types.

```haskell
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show) 
```

Notice that when defining a point, we used the same name for the data type and the value constructor. This has no special meaning, although it's common to use the **same name as the type if there's only one value constructor**.

```haskell
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

This makes it easier to understand what's what. 

```haskell
ghci> surface (Rectangle (Point 0 0) (Point 100 100))  
10000.0  
ghci> surface (Circle (Point 0 0) 24)  
1809.5574
```

```haskell
nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  
```

```haskell
ghci> nudge (Circle (Point 34 34) 10) 5 10  
Circle (Point 39.0 44.0) 10.0
```

```haskell
baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)
```
  
```haskell  
ghci> nudge (baseRect 40 100) 60 23  
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
```

### Exporting Types
[index](#index)

You can, of course, export your data types in your modules. To do that, just write your type along with the functions you are exporting and then add some parentheses and in them specify the value constructors that you want to export for it, separated by commas. If you want to export all the value constructors for a given type, just write `..`.

If we wanted to export the functions and types that we defined here in a module, we could start it off like this:

```haskell
module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  
```

By doing `Shape(..)`, we exported all the value constructors for `Shape`, so that means that whoever imports our module can make shapes by using the `Rectangle` and `Circle` value constructors. It's the same as writing `Shape (Rectangle, Circle)`.

We could also opt not to export any value constructors for `Shape` by just writing `Shape` in the export statement. That way, someone importing our module could only make shapes by using the auxilliary functions `baseCircle` and `baseRect`.
`Data.Map` uses that approach. You can't create a map by doing `Map.Map [(1,2),(3,4)]` because it doesn't export that value constructor. However, you can make a mapping by using one of the auxilliary functions like `Map.fromList`.

## [Record syntax ](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax)
[index](#index)

Instead of writing like this

```haskell
data Person = Person String String Int Float String String deriving (Show)  
```

```haskell
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
ghci> guy  
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height 
```

Cumbersome!
We can write it like this

```haskell
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)
```

The main benefit of this is that it creates functions that lookup fields in the data type. By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor.

```haskell
ghci> :t flavor  
flavor :: Person -> String  
ghci> :t firstName  
firstName :: Person -> String  
```

Another advantage is how `Show` is derived.

```haskell
data Car = Car String String Int deriving (Show)  

ghci> Car "Ford" "Mustang" 1967  
Car "Ford" "Mustang" 1967  
```

```haskell
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  

ghci> Car {company="Ford", model="Mustang", year=1967}  
Car {company = "Ford", model = "Mustang", year = 1967} 
```

## [Type paramters](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-parameters)
[index](#index)

A **value constructor** can take some **values parameters** and then **produce a new value**. For instance, the Car constructor takes three values and produces a car value. In a similar manner, **type constructors** can take types as parameters to **produce new types**.

```haskell
data Bool   = False | True  
--type constructor =
--type name = value constructor
```

```haskell
data Maybe a = Nothing | Just a  
           --type parameter (a)
               --value constructor (after =)
```

The `a` here is the **type parameter**. And because there's a **type parameter** involved, we call `Maybe` a **type constructor**. Depending on what we want this data type to hold when it's not `Nothing`, this **type constructor** can end up producing a type of `Maybe Int`, `Maybe Car`, `Maybe String`, etc. No value can have a type of `just Maybe`, because that's not a type per se, it's a **type constructor**. In order for this to be a real type that a value can be part of, it has to have all its **type parameter filled up.

So if we pass `Char` as the **type parameter** to `Maybe`, we get a type of `Maybe Char`. The value `Just 'a'` has a type of `Maybe Char`, for example.

We used a **type parameter** before. `[]` values can have an `[Int]` type, a `[Char]` type, a `[[String]]` type, but you can't have a value that just has a type of `[]`.

Using type parameters is very beneficial, but only when using them makes sense. Usually we use them when our data type would work regardless of the type of the value it then holds inside it, like with our `Maybe a` type. If our type acts as some kind of box, it's good to use them. We could change our Car data type from this:

```haskell
data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show)  
```

```haskell
data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)
```

parameterizing the Car type isn't really worth it.

Never add typeclass constraints in data declarations. 
So don't put type constraints into data declarations even if it seems to make sense, because you'll have to put them into the function type declarations either way.

```haskell
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
```

We used a *parameterized type* because even though it will usually contain numeric types, it will still support *several* of them.

## [Derived Instances]()
[index](#index)

In other imperative languages classes are a blueprint from which we then create objects that contain state and can do some actions. **Typeclasses** are more like *interfaces*. We don't make data from typeclasses. Instead, we first make our data type and then we think about what it can act like. If it can act like something that can be equated, we make it an instance of the `Eq` typeclass. If it can act like something that can be ordered, we make it an instance of the `Ord` typeclass.

```haskell
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq)  
```

```haskell
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}  
ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}  
ghci> mca == adRock  
False  
ghci> mikeD == adRock  
False  
ghci> mikeD == mikeD  
True  
ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}  
True
```

Of course, since Person is now in `Eq`, we can use it as the a for all functions that have a class constraint of `Eq` a in their type signature, such as `elem`.

```haskell
ghci> let beastieBoys = [mca, adRock, mikeD]  
ghci> mikeD `elem` beastieBoys  
True  
```

The `Show` and `Read` typeclasses are for things that can be converted **to** or **from** strings.

```haskell
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
ghci> mikeD  
Person {firstName = "Michael", lastName = "Diamond", age = 43}  
ghci> "mikeD is: " ++ show mikeD  
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
```

```haskell
ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person  
Person {firstName = "Michael", lastName = "Diamond", age = 43}
```

We can also read parameterized types, but we have to fill in the type parameters. So we can't do `read "Just 't'" :: Maybe a`, but we can do `read "Just 't'" :: Maybe Char`.

The value which was made with a constructor that's defined first is considered smaller. 

```haskell
data Bool = False | True deriving (Ord)
```

```haskell
ghci> True `compare` False  
GT  
ghci> True > False  
True  
ghci> True < False  
False
```

In the `Maybe` a data type, the `Nothing` value constructor is specified before the `Just` value constructor, so a value of `Nothing` is always smaller than a value of `Just something`, even if that something is minus one billion trillion. But if we compare two Just values, then it goes to compare what's inside them.

```haskell
ghci> Nothing < Just 100  
True  
ghci> Nothing > Just (-49999)  
False  
ghci> Just 3 `compare` Just 2  
GT  
ghci> Just 100 > Just 50  
True  
```

But we can't do something like `Just (*3) > Just (*2)`, because `(*3)` and `(*2)` are functions, which aren't instances of `Ord`.

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  
```

Because all the value constructors are nullary (take no parameters, i.e. fields), we can make it part of the `Enum` typeclass. The `Enum` typeclass is for things that have predecessors and successors. We can also make it part of the `Bounded` typeclass, which is for things that have a lowest possible value and highest possible value. And while we're at it, let's also make it an instance of all the other derivable typeclasses and see what we can do with it.

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

Because it's part of the `Show` and `Read` typeclasses, we can convert values of this type to and from strings.

```haskell
ghci> Wednesday  
Wednesday  
ghci> show Wednesday  
"Wednesday"  
ghci> read "Saturday" :: Day  
Saturday  
```

Because it's part of the `Eq` and `Ord` typeclasses, we can compare or equate days.

```haskell
ghci> Saturday == Sunday  
False  
ghci> Saturday == Saturday  
True  
ghci> Saturday > Friday  
True  
ghci> Monday `compare` Wednesday  
LT  
```

It's also part of `Bounded`, so we can get the lowest and highest day.

```haskell
ghci> minBound :: Day  
Monday  
ghci> maxBound :: Day  
Sunday  
```

It's also an instance of `Enum`. We can get predecessors and successors of days and we can make list ranges from them!

```haskell
ghci> succ Monday  
Tuesday  
ghci> pred Saturday  
Friday  
ghci> [Thursday .. Sunday]  
[Thursday,Friday,Saturday,Sunday]  
ghci> [minBound .. maxBound] :: [Day]  
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]  
```

## [Type synonyms](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-synonyms)
[index](#index)

The `[Char]` and `String` types are equivalent and interchangeable. That's implemented with type synonyms.

Type synonyms don't really do anything per se, they're just about giving some types different names so that they make more sense to someone reading our code and documentation.

```haskell
type String = [Char]
```

Giving the String type synonyms is something that Haskell programmers do when they want to convey more information about what strings in their functions should be used as and what they represent.

```haskell
phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]  
```

```haskell
type PhoneBook = [(String,String)]
```

```haskell
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)] 
```

```haskell
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook 
```

If we decided not to use type synonyms, our function would have a type of `String -> String -> [(String,String)] -> Bool`.

Type synonyms can also be parameterized. If we want a type that represents an association list type but still want it to be general so it can use any type as the keys and values, we can do this:

```haskell
type AssocList k v = [(k,v)]  
```

Now, a function that gets the value by a key in an association list can have a type of `(Eq k) => k -> AssocList k v -> Maybe v`. `AssocList` is a **type constructor** that takes two types and produces a **concrete type**, like `AssocList Int String`, for instance.

When we say **concrete types** we mean like fully applied types. like `Map Int String` or if we're dealing with polymorphic functions, `[a] or (Ord a) => Maybe a`. Sometimes we say that `Maybe` is a type, but we don't mean that, cause Maybe is a **type constructor**. When I apply an extra type to `Maybe`, like `Maybe String`, then I have a **concrete type**. You know, *values* can only have types that are **concrete types**.

### Partial *type constructor*
[index](#index)

Just like we can partially apply functions to get new functions, we can partially apply type parameters and get new type constructors from them. Just like we call a function with too few parameters to get back a new function, we can specify a type constructor with too few type parameters and get back a partially applied type constructor.

If we wanted a type that represents a map (from `Data.Map`) from integers to something..

```haskell
Prelude> import Data.Map as Map
*Main> map = Map.fromList [("uno",7),("due",6)]
*Main> :t map
map :: Num a => Map.Map [Char] a
```

.. we could either do this:

```haskell
type IntMap v = Map Int v  
```

Or we could do it like this:

```haskell
type IntMap = Map Int  
```

Either way, the `IntMap` type constructor takes one parameter and that is the type of what the integers will point to (as key).

Type synonyms (and types generally) can only be used in the type portion of Haskell. We're in Haskell's type portion whenever we're defining new types (so in data and type declarations) or when we're located after a `::`. The `::` is in type declarations or in type annotations.

### Either a b

```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show) 
```

```haskell
ghci> Right 20  
Right 20  
ghci> Left "w00t"  
Left "w00t"  
ghci> :t Right 'a'  
Right 'a' :: Either a Char  
ghci> :t Left True  
Left True :: Either Bool b  
```

when we're interested in how some function failed or why, we can't use `Maybe` (`Nothing` doesn't hold info), we usually use the result type of `Either a b`, where `a` is some sort of type that can tell us something about the possible failure and `b` is the type of a successful computation. Hence, **errors** use the `Left` value constructor while **results** use `Right`.

Here is an example with students lockers for the locker supervisor.

```haskell
import qualified Data.Map as Map  
  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code) 
```

We're going to use an `Either String Code` type to represent our result, because our lookup can fail in two ways — the locker can be taken, in which case we can't tell the code or the locker number might not exist at all.

```haskell
lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"  
```

```haskell
lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  
```

```haskell
ghci> lockerLookup 101 lockers  
Right "JAH3I"  
ghci> lockerLookup 100 lockers  
Left "Locker 100 is already taken!"  
ghci> lockerLookup 102 lockers  
Left "Locker number 102 doesn't exist!"  
ghci> lockerLookup 110 lockers  
Left "Locker 110 is already taken!"  
ghci> lockerLookup 105 lockers  
Right "QOTSA"  
```

We could have used a Maybe a to represent the result but then we wouldn't know why we couldn't get the code.

## [Recusive Data Structure](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures)
[index](#index)

Also called **Algebraic Data Structures**.

Think about this list: `[5]`. That's just syntactic sugar for `5:[]`. On the left side of the `:`, there's a value and on the right side, there's a list. And in this case, it's an empty list. Now how about the list `[4,5]`? Well, that desugars to `4:(5:[])`. Looking at the first `:`, we see that it also has an element on its left side and a list (`5:[]`) on its right side. Same goes for a list like `3:(4:(5:6:[]))`, which could be written either like that or like `3:4:5:6:[]` (because : is right-associative) or `[3,4,5,6]`.

We could say that a list can be an *empty list* or it can be an *element* joined together with a `:` with *another list* (that can be either the empty list or not).

```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  
```

```haskell
Prelude> xs = Cons 1
Prelude> :t xs
xs :: Num a => List a -> List a
Prelude> xs xs
<interactive>:5:4: error:
    • Couldn't match expected type ‘List a’
                  with actual type ‘List a0 -> List a0’
```

```haskell
Prelude> x = xs
Prelude> xs = x Empty
Prelude> :t xs
xs :: Num a => List a
```

Cons is just a constructor (any other word could be used as with Empty). cons is another word for `:`. `:` is actually a constructor that takes a value and another list and returns a list. We can use our version of list with Cons, one field is of the type of `a` and the other is of the type `[a]`.

```haskell
ghci> Empty  
Empty  
ghci> Cons 5 Empty  
Cons 5 Empty  
ghci> Cons 4 (Cons 5 Empty)  
Cons 4 (Cons 5 Empty)  
ghci> Cons 3 (Cons 4 (Cons 5 Empty))  
Cons 3 (Cons 4 (Cons 5 Empty)) 
--can also write as infix to be more similar to :
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty)) 
Cons 3 (Cons 4 (Cons 5 Empty)) 
```

We can define functions to be automatically infix by making them of only special characters. We can also do the same with constructors, since they're just functions that return a data type.

```haskell
infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord) 
```

The `infixir` is a fixity that states how tightly the operator binds and whether it's left-associative or right-associative. It is optional. higher number means higher bind.

```haskell
ghci> 3 :-: 4 :-: 5 :-: Empty  
(:-:) 3 ((:-:) 4 ((:-:) 5 Empty))  
ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
ghci> 100 :-: a  
(:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))
```

When deriving Show for our type, Haskell will still display it as if the constructor was a prefix function, hence the parentheses around the operator (remember, `4 + 3` is `(+) 4 3`).

Let's make our own funs `++`.

```haskell
infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)  
```

as 

```haskell
Prelude> :t (++)
(++) :: [a] -> [a] -> [a]
```

```haskell
ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
ghci> let b = 6 :-: 7 :-: Empty  
ghci> a .++ b  
(:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty)))) 
```

Notice how we pattern matched on `(x :-: xs)`. That works because pattern matching is actually about matching constructors. We can match on `:-:` because it is a constructor for our own list type and we can also match on `:` because it is a constructor for the built-in list type.

### binary search tree
[index](#index)

```
    5
 3     7
1 4   6 8
```

If are looking for 8, in normal lists you would need 7 steps, but with the tree that has smaller numbers on the left and bigger on the right you only need two hops.
This method is used in `Data.Set` and `Data.Map` hence wy they are quicker than lists. (they actually use balanced binary search trees).

Here is the model for it

```haskell
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 
```

```haskell
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x              -- edge condition
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 
```

```haskell
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 
```

And now instead of buildign a tree manually we are going to build it with foldr.

```haskell
ghci> let nums = [8,6,4,1,7,3,5]  
ghci> let numsTree = foldr treeInsert EmptyTree nums  
ghci> numsTree  
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))  
```

```haskell
ghci> 8 `treeElem` numsTree  
True  
ghci> 100 `treeElem` numsTree  
False  
ghci> 1 `treeElem` numsTree  
True  
ghci> 10 `treeElem` numsTree  
False  
```

## [Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#typeclasses-102)
[index](#index)

This is how the `Eq` class is defined in the standard prelude:

```haskell
class Eq a where  
    (==) :: a -> a -> Bool         --type declarations.
    (/=) :: a -> a -> Bool         --type declarations.  
    x == y = not (x /= y)          --function body. 
    x /= y = not (x == y)          --function body. 
```

```haskell
ghci> :t (==)
(Eq a) => a -> a -> Bool           --functions in class will have type with constraint.
```

`class Eq a were` here we define the new class by giving a name `Eq`.
`a` is a type variable, it plays the role of the type that will be an instance of `Eq`. (could have been called `equitable` would have been OK, just need to be lower caps)
It's not mandatory to implement the function bodies themselves, we just have to specify the type declarations for the functions.

Let's instantiate a new type without deriving it.

```haskell
data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False 
```

We defined the functions in the function bodies in terms of mutual recursion. Because `==` was defined in terms of `/=` and vice versa in the class declaration, we only had to overwrite one of them in the instance declaration. 
That's called the **minimal complete definition** for the typeclass — the minimum of functions that we have to implement so that our type can behave like the class advertises. To fulfill the **minimal complete definition** for `Eq`, we have to overwrite either one of `==` or `/=`. If `Eq` was defined simply like this:

```haskell
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
```

we'd have to implement both of these functions when making a type an instance of it, because Haskell wouldn't know how these two functions are related. The minimal complete definition would then be: both `==` and `/=`.

You can see that we implemented `==` simply by doing pattern matching. Since there are many more cases where two lights aren't equal, we specified the ones that are equal and then just did a catch-all pattern saying that if it's none of the previous combinations, then two lights aren't equal.

```haskell
Prelude> :i Eq
type Eq :: * -> Constraint
class Eq a where
  (Prelude.==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}        -- "/" as in Or
```

Minimal complete definition by the way only means that there must be at least one pattern for each minimal function.
But if you for instance don't put the `Yellow == Yellow` option, the definition will go through, but you will get:

```haskell
Prelude> Yellow == Yellow
*** Exception: <interactive>:(76,5)-(77,25): Non-exhaustive patterns in function ==
```

Let's make this an instance of `Show` by hand, too. To satisfy the minimal complete definition for `Show`, we just have to implement its show function, which takes a value and turns it into a string.

```haskell
Prelude> :i Show
type Show :: * -> Constraint
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
```

```haskell
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  
```

```haskell
ghci> Red == Red  
True  
ghci> Red == Yellow  
False  
ghci> Red `elem` [Red, Yellow, Green]  
True  
ghci> [Red, Yellow, Green]  
[Red light,Yellow light,Green light]    
```

### Subclasses
[index](#index)

**in class declarations**

You can also make typeclasses that are subclasses of other typeclasses.

```haskell
class (Eq a) => Num a where  
   ...  
```

Mind that this is not a type, but a classtype definition, so in the class constraint we're essentially saying that we have to make `a` type an instance of `Eq` before we can make it an instance of `Num`.

That's all there is to **subclassing** really, it's just a **class constraint** on a **class declaration**! When defining function bodies in the class declaration or when defining them in instance declarations, we can assume that `a` is a part of `Eq` and so we can use `==` on values of that type.

**in instance declarations**

```haskell
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)
```

```haskell
instance Eq Maybe where  
    ...    
```

Because like we've seen, the `a` has to be a concrete type but `Maybe` isn't a concrete type. It's a type constructor that takes one parameter and then produces a concrete type. It would also be tedious to write `instance Eq (Maybe Int) where`, `instance Eq (Maybe Char) where`, etc. for every type ever. So we could write it out like so:

```haskell
instance Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False  
```

We actually could have written (`Maybe something`), but we usually opt for single letters to be true to the Haskell style.
By specifying a type parameter (`m`, which is in lowercase), we said that we want all types that are in the form of `Maybe m`, where `m` is any type, to be an instance of `Eq`.

There's one problem with this though. Can you spot it? We use `==` on the contents of the `Maybe` but we have no assurance that what the `Maybe` contains can be used with `Eq`! That's why we have to modify our instance declaration like this:

```haskell
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False  
```
        
We had to add a *class constraint*! With this *instance declaration*, we say this: we want all types of the form `Maybe m` to be part of the `Eq` *typeclass*, but only those types where the `m` is also a part of `Eq`.

Most of the times, ***class constraints* in *class declarations* are used for making a *typeclass* a *subclass* of another *typeclass*** and ***class constraints* in *instance declarations* are used to express requirements about the contents of some type**. For instance, here we required the contents of the `Maybe` (`m`) to also be part of the `Eq` typeclass.

## [YesNo Typeclass Instantiate Example](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#a-yes-no-typeclass)
[index](#index)

```haskell
class YesNo a where  
    yesno :: a -> Bool
```

```haskell
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True

instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True

instance YesNo Bool where  
    yesno = id 
```

What's `id`? It's just a standard library function that takes a parameter and returns the same thing.

```haskell
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False 
```

`Maybe -> Bool` function can't exist (because Maybe isn't a concrete type), whereas a `Maybe a -> Bool` is fine, hence why `(Maybe a)`.

```haskell
instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True 
```

from [binary search tree](#binary-search-tree).


```haskell
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True 
```

from [TrafficLight](#typeclasses).

```haskell
ghci> yesno $ length []  
False  
ghci> yesno "haha"  
True  
ghci> yesno ""  
False  
ghci> yesno $ Just 0  
True  
ghci> yesno True



True  
ghci> yesno EmptyTree  
False  
ghci> yesno []  
False  
ghci> yesno [0,0,0]  
True  
ghci> :t yesno  
yesno :: (YesNo a) => a -> Bool  
```

```haskell
yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult 
```

```haskell
ghci> yesnoIf [] "YEAH!" "NO!"  
"NO!"  
ghci> yesnoIf [2,3,4] "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf True "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf (Just 500) "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf Nothing "YEAH!" "NO!"  
"NO!"
```

## [The Functor typeclass](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass)
[index](#index)

It is basically for things that can be mapped over. Lists for isntance are part of the `Functor` classtype.

```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b 
```

One function is defined, `fmap`, but there is not any implementation for it.
If you notice the `f` is not a normal type variable like `a` is for example in `(==) :: (Eq a) => a -> a -> Bool`.
So far the *type variables* we've seen were **concrete type variables** like `a` (basically a *type* that a *value* can hold like `Int`, `String` or `Maybe Int`) (A quick refresher example: `Maybe Int` is a *concrete type*, but `Maybe` is a *type constructor* that takes one *type parameter*).
`f` is not a **concrete type** but a **type constructor** that takes in a **type parameter**.

`Functor`'s `fmap` si very similar to

```haskell
ghci> :t map
map :: (a -> b) -> [a] -> [b]
```

In fact, `map` is just a `fmap` that works only on lists.

For historic reasons, `map` is the mapping function specialized for lists as defined by prelude, while `fmap` is the general mapping function for functors. Of course, since lists are a type of functor, when defining lists as a member of the functor typeclass, the `fmap` function for lists is defined as `map`,

fmap has a more general type and can be applied to any functor:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

While the type of map is specialized for lists:

```haskell
map :: (a -> b) -> [a] -> [b]
```

Last, but not least, if you look at the definition of the list instance of functor, you can verify that `fmap` is just `map`:

```haskell
instance Functor [] where  
    fmap = map
```

This is the actual source from GHC.base. [source](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Base.html#line-1148)
Notice how we didn't write instance `Functor [a] where`, because from `fmap :: (a -> b) -> f a -> f b`, we see that the f has to be a *type constructor* that takes one type. `[a]` is already a *concrete type* (of a list with any type inside it), while `[]` is a *type constructor* that takes one type and can produce types such as `[Int]`, `[String]` or even `[[String]]`.

Since for lists, `fmap` is just `map`, we get the same results when using them on lists.

```haskell
map :: (a -> b) -> [a] -> [b]  
ghci> fmap (*2) [1..3]  
[2,4,6]  
ghci> map (*2) [1..3]  
[2,4,6]  
```

What happens when we `map` or `fmap` over an empty list `[]`? Well, of course, we get an empty list. It just turns an empty list of type `[a]` into an empty list of type `[b]`.

Types that can act like a box can be `Functor`s. *List* is like an infinite box with infinite compartments, while `Maybe` is a box that can hold nothing in which case it will be `Nothing` or can old a value of any type.

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing 
```

Again, notice how we wrote instance `Functor Maybe` where instead of instance `Functor (Maybe m) where`. `Functor` wants a *type constructor* that takes one type and not a *concrete type*. If you mentally replace the `f`s with `Maybes`, `fmap` acts like a `(a -> b) -> Maybe a -> Maybe b` for this particular type, which looks OK. But if you replace `f` with `(Maybe m)`, then it would seem to act like a `(a -> b) -> Maybe m a -> Maybe m b`, which doesn't make any damn sense because `Maybe` takes just one type parameter.

Anyway, the fmap implementation is pretty simple. If it's an empty value of `Nothing`, then just return a `Nothing`. If we map over an empty box, we get an empty box. It makes sense. Just like if we map over an empty list, we get back an empty list. If it's not an empty value, but rather a single value packed up in a `Just`, then we apply the function on the contents of the `Just`.

```haskell
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  
Just "Something serious. HEY GUYS IM INSIDE THE JUST"  
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing  
Nothing  
ghci> fmap (*2) (Just 200)  
Just 400  
ghci> fmap (*2) Nothing  
Nothing 
```

Another thing that can be mapped over and made an instance of `Functor` is our `Tree a` type. It can be thought of as a box in a way (holds several or no values) and the `Tree` type constructor takes exactly one *type parameter*.

`fmap` signature applied to `Tree` would look like `(a -> b) -> Tree a -> Tree b`.

```haskell
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 
```

```haskell
ghci> fmap (*2) EmptyTree  
EmptyTree  
ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])  
Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree  
```

### Two Type Parameters Functor
[index](#index)

The `Functor` typeclass wants a type constructor that takes only one type parameter but `Either` takes two. Hmmm! I know, we'll partially apply `Either` by feeding it only one parameter so that it has one free parameter. Here's how `Either a` is a functor in the standard libraries:

```haskell
instance Functor (Either a) where  
    fmap f (Right x) = Right (f x)  
    fmap f (Left x) = Left x 
```

If `fmap` was specifically for `Either a`, the type signature would then be `(b -> c) -> Either a b -> Either a c` because that's the same as `(b -> c) -> (Either a) b -> (Either a) c`.

In the implementation, we mapped in the case of a `Right` value constructor, but we didn't in the case of a `Left`.
To see why let's look back to `Either`'s definition

```haskell
data Either a b = Left a | Right b  
```

If we wanted to map one function over both of them, `a` and `b` would have to be the same type.
Also, from seeing what `fmap`'s type would be if it operated only on Either values (`(b -> c) -> Either a b -> Either a c`), we see that the first parameter (`a`) has to remain the same while the second one (`b`) can change (to `c`) and the first parameter (`a`) is actualized by the `Left` value constructor.

This also goes nicely with our box analogy if we think of the `Left` part as sort of an empty box with an error message written on the side telling us why it's empty.

Maps from `Data.Map` can also be made a functor because they hold values (or not). In the case of `Map k v`, `fmap` will map a function `v -> v'` over a map of type `Map k v` and return a map of type `Map k v'`. `(v -> v') -> Map k v -> Map k v'`.

## [Kind, Types and Values](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#kinds-and-some-type-foo)
[index](#index)

**Type constructors** take other **types** as *parameters* to eventually produce **concrete types**. That kind of reminds me of **functions**, which take **values** as *parameters* to produce **values**.

```
Type Constructors -> Types  -> Concrete Types
Functions         -> Values -> Values'
```

We've seen that **type constructors** can be partially applied (`Either String` is a *type* that takes one type and produces a **concrete type**, like `Either String Int`), just like functions can.

We'll take a look at formally defining how *types* are applied to **type constructors**, just like we took a look at formally defining how *values* are applied to **functions** by using type declarations. 

Values like `3`, `"YEAH"` or `takeWhile` (functions are also values, because we can pass them around and such) each have their own *type*. *Types* are little labels that values carry so that we can reason about the *values*. But *types* have their own little labels, called *kinds*. **A *kind* is more or less the type of a type**.

Different levels of abstraction.

```
type [ values ]
kind [ types  ]

kind [ type [ value ] ] 
```

To see what kind a type has:

```haskell
ghci> :k Int  
Int :: *
```

A `*` means that the type is a *concrete type*. A *concrete type* is a type that doesn't take any *type parameters*. *Values* can only have *types* that are *concrete types*.

```haskell
ghci> :k Maybe  
Maybe :: * -> *
```

`* = Concrete Type` The `Maybe` *type constructor* takes one *concrete type* (like `Int`) and then returns a *concrete type* like `Maybe Int`. And that's what this *kind* tells us. Just like `Int -> Int` means that a *function* takes an `Int` and returns an `Int`, `* -> *` means that the *type constructor* takes one *concrete type* and returns a *concrete type*.

```haskell
ghci> :k Maybe Int  
Maybe Int :: * 
```

We applied the *type parameter* to `Maybe` and got back a *concrete type* (that's what `* -> *` means).
If we do `:t isUpper` has a *type* of `Char -> Bool` and `:t isUpper 'A'` has a *type* of `Bool`. Both those *types*, however, have a *kind* of `*`.

```haskell
ghci> :k Either  
Either :: * -> * -> *
```

Type constructors are <ins>curried</ins> (just like functions), so we can partially apply them.

```haskell
ghci> :k Either String
Either String :: * -> *
ghci> :k Either String Int
Either String Int :: *
```

When we wanted to make `Either` a part of the `Functor` typeclass, we had to partially apply it because `Functor` wants types that take only one parameter while `Either` takes two. In other words, `Functor` wants types of kind `* -> *` and so we had to partially apply `Either` to get a type of kind `* -> *` instead of its original kind `* -> * -> *`. If we look at the definition of `Functor` again

```haskell
class Functor f where   
    fmap :: (a -> b) -> f a -> f b
```

We see that the `f` *type variable* is used as a type that takes one *concrete type* (`a`) to produce a *concrete type* (`f b`). We know it has to produce a *concrete type* because it's used as the type of a value in a function (`(a -> b)`, `f a`, `f b` must all be *concrete types* `*` because they're all values in a function). And from that, we can deduce that types that want to be friends with `Functor` have to be of kind `* -> *`.

### Kind Types Forge - Type-Foo 1
[index](#index)

Let's experiment makign this funs up

```haskell
class Tofu t where  
    tofu :: j a -> t a j
```

`(* -> *) -> * -> (* -> *) -> *`

`j a` is used as the type of a value that the `tofu` function takes as its parameter, `j a` has to have a kind of `*`.
We assume `*` for `a` and so we can infer that `j` has to have a kind of `* -> *`. 
We see that `t` has to produce a concrete value too and that it takes two types. And knowing that `a` has a kind of `*` and `j` has a kind of `* -> *`, we infer that `t` has to have a kind of `* -> (* -> *) -> *`.
So it takes a concrete type (`a`) (wiht kind `*`), a type constructor that takes one concrete type (`j`) (with kind `(* -> *)`) and produces a concrete type (`*`).

> To define a *kind* of a type `t` in `t a j`, you remove `t` and define the kinds of each type parameter (`a` = `*`, `j` = `(* -> *)`) and add the produced final concrete type (`*`).

So we need now to forge the type `t` whose kind we found to be `* -> (* -> *) -> *`. Here's one way of going about it.

```haskell
data Frank a b  = Frank {frankField :: b a} deriving (Show)
```

Fields representation are made to hold values, so they must be of kind `*`. So we assume `*` for `a`, which means that `b` takes one type parameter and so its kind is `* -> *`.
So we now know `a` and `b`, which being parameters of `Frank` we can tell its kind is returning `*` preceded by `a`'s `*` and `b`'s `* -> *`. So `Frank` kind = `* -> (* -> *) -> *`.
Let's make some Frank values and check out their types.

```haskell
ghci> :t Frank {frankField = Just "HAHA"}
Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
ghci> :t Frank {frankField = "YES"}
Frank {frankField = "YES"} :: Frank Char []
```

> notice how `a` and `b` values are flipped when it comes to the type as per `data` definition.

let's make `Frank` an instance of `Tofu`

```haskell
class Tofu t where  
    tofu :: j a -> t a j

data Frank a b  = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where  
    tofu x = Frank x 
```

```haskell
ghci> tofu (Just 'a') :: Frank Char Maybe  
Frank {frankField = Just 'a'}  
ghci> tofu ["HELLO"] :: Frank [Char] []  
Frank {frankField = ["HELLO"]}  
```

Not very useful, but we did flex our type muscles. Let's do some more type-foo. We have this data type:

### Kind Types Forge - Type-Foo 2

```haskell
data Barry t k p = Barry { yabba :: p, dabba :: t k } 
```

We want to make `Barry` an instance of `Functor`. `Functor` wants types of kind `* -> *` but `Barry` doesn't look like it has that kind.
`Barry` takes three **type parameters**, so it's going to be `something -> something -> something -> *`.
It's safe to say that `p` is a **concrete type** and thus has a kind of `*`.
For `k`, we assume `*` and so by extension, `t` has a kind of `* -> *`. 
Replacing the placeholders the result is `(* -> *) -> * -> * -> *`.

```haskell
ghci> :k Barry  
Barry :: (* -> *) -> * -> * -> * 
```

To make `Barry` a part of `Functor` we have to partially apply the first two type parameters so that we're left with `* -> *`. That means that the start of the instance declaration will be: instance Functor `(Barry a b) where`.
If we look at `fmap` as if it was made specifically for `Barry`, it would have a type of `fmap :: (a -> b) -> Barry c d a -> Barry c d b`, because we just replace the `Functor`'s `f` with `Barry c d`. 
The third type parameter from `Barry` will have to change and we see that it's conviniently in its own field.

```haskell
class Functor f where   
    fmap :: (a -> b) -> f a -> f b

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
--                     p (a)    t k (b)                p (a)    t k (b) 
--  fmap :: (a -> b) -> Barry c d a -> Barry c d b
--              f       Barry x y      Barry x y 
```

> the above  is hard to truly understand and in the end not is usefull as we are practising type-foo, not really things we'll encounter often.

## [(!)](https://stackoverflow.com/questions/993112/what-does-the-exclamation-mark-mean-in-a-haskell-declaration)

it's a strictness declaration. Basically, it means that it must be evaluated to what's called "weak head normal form" when the data structure value is created.

```haskell
f x !y = x*y
```

`f (1+1) (2+2)` will return the thunk `(1+1)*4`.

it forces the execution earlier on at thunk level.

## [Vectors](https://mmhaskell.com/data-structures/vector)

in Vectors `!` is used to take an element at a certain index.

## Monads

The monad is a type wiht an *AND THEN* operator.

```haskell
class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a
```

Imagine givng instructions to get out of a room an operator. 

a: Do this

b: and then?

a: Do that

b: and then?

a: Do thut

b: and then?

a: return. 

<br>

now imagine a series of instructions do get out of a room:

a: Check the desk, reverse the letters on the next clue

c: Open the reward.

a: So you look in the drawer

c: in the drawer there is a key that says unlock the door

a: You can unlock the door.

This process is elegantly coded wiht a Monad.

Let put the lues in a [location, clue] pairs.

```haskell
al :: [(String, String)]
al = [("desk",   "reward")
     ,("key",    "door"  )
     ,("door",   "escape")
     ,("drawer", "key"   )]
```

To find the next function we can simply use the Prelude function `lookup` to find the next clue.

```haskell
lookup :: Eq a => [(a, b)] -> Maybe b
```

`lookup` returns `Nothing` if nothing was found.

Let's build out own lookup4 that tries to simulate the task of getting out of the room.

```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = lookup k1 al
```

we look for the first key in `al`.

In the case nothing is found we return `Nothing`.

```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = case lookup k1 al of
    Nothing -> Nothing
```

if instead the `lookup k1 al` returns a `Just key` we do another lookup:

```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = case lookup k1 al of
    Nothing -> Nothing
    Just k2 -> lookup (reverse k2) al
```

But this second lookup may in turn fail or return another key.

```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = case lookup k1 al of
    Nothing -> Nothing
    Just k2 -> case lookup (reverse k2) al of
        Nothing -> Nothing
        Just k3 -> lookup k3 al
```

and so on all the way to the 4th key

```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = case lookup k1 al of
    Nothing -> Nothing
    Just k2 -> case lookup (reverse k2) al of
        Nothing -> Nothing
        Just k3 -> case lookup k3 al of
            Nothing -> Nothing
            Just k4 -> case lookup k4 of
                Nothing -> Nothing
                Just s -> Just ("You " ++ s)
```

In the last key 4 we return a message of success if the key was found or Nothing.

We can see that there is a pattern repeating

```haskell
Nothing -> Nothing
    Just k* -> case lookup k* al of
```

but te `reverse` spoils it in the second key.

At the basic level we are usign this logic:

- Check the `Maybe`
- If is `Nothing` quit.
- Otherwise extract Key from the `Just` and pass it to the next step.

let's introduce a `ifJust` function that does exactly that.

```haskell
ifJust :: Maybe String -> (String -> Maybe String) -> Maybe String
ifJust Nothing _  = Nothing
ifJust (Just k) f = f k
```

It takes a `Maybe String` resulting from the latest `lookup` and a function that takes a `String` to look up and produces a `Maybe String`.
Overall it will also produce a `Maybe String` itself.

We see that the implementation is quite simple, it returns Nothing if Nothing was found as in the Maybe given was Nothing, otherwise it applies the String given from the Maybe to the function supplied as second argument.

```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = case lookup k1 al of
    Nothing -> Nothing
    Just k2 -> case lookup (reverse k2) al of
        Nothing -> Nothing
        Just k3 -> case lookup k3 al of
            Nothing -> Nothing
            Just k4 -> case lookup k4 of
                Nothing -> Nothing
                Just s -> Just ("You " ++ s)
```

```haskell
ifJust :: Maybe String -> (String -> Maybe String) -> Maybe String
ifJust Nothing _  = Nothing
ifJust (Just k) f = f k

lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = 
    ifJust (lookup k1 al) 
        (\k2 -> ifJust (lookup (reverse k2) al) 
        (\k3 -> ifJust (lookup k3 al) 
        (\k4 -> ifJust (Just k4 al) 
        (\s  -> Just ("You " ++ s)))))
```

It becomes more readable if we use `ifJust` as a binary operator ``ifJust``

```haskell
ifJust :: Maybe String -> (String -> Maybe String) -> Maybe String
ifJust Nothing _  = Nothing
ifJust (Just k) f = f k

lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = 
    lookup k1 al                  `ifJust`  
    \k2 -> lookup (reverse k2) al `ifJust`  
    \k3 -> lookup k3 al           `ifJust`  
    \k4 -> lookup k4 al           `ifJust`
    \s  -> Just ("You " ++ s)
```

The above is basically similar to

```js
lookup(k1, al)
    .then(k2 -> lookup(reverse(k2), al))
    .then(k3 -> lookup(k3, al))
    .then(k4 -> lookup(k4, al))
    .then(s -> just("you" ++ s))
    .catch(() -> null)
```

The central operation of a Monad is a function called **bind** `>>=` which type signature matches that of our `ifJust`.

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

```haskell
ifJust :: Maybe String -> (String -> Maybe String) -> Maybe String
```

So we can avoid to use `ifJust` and use the bind function directly `>>=`.

```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = 
    lookup k1 al                  >>=  
    \k2 -> lookup (reverse k2) al >>=  
    \k3 -> lookup k3 al           >>=  
    \k4 -> lookup k4 al           >>=
    \s  -> Just ("You " ++ s)
```

> Notice that `Maybe` in an instance of the `Monad` typeclass

```haskell
instance Monad Maybe where
    Nothing  >>= _ = Nothing
    (Just x) >>= k = k x
``` 

Using Monads is such a convenient and common thing that we actually have used already. Haskell has a simple syntax for them, with the `do` blocks.

> wrong use of `do` block:
```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = do lookup k1 al 
                   k2 -> lookup (reverse k2) al
                   k3 -> lookup k3 al
                   k4 -> lookup k4 al
                   s  -> Just ("You " ++ s)
```

The `do` block automagically inserts the lambda `\` and bind `>>=` opration for you.

**But** the only difference is that you write the **argument** of the next step **before** each step rather than after.

> almost correct use:
```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = do k2 <- lookup k1 al 
                   k3 <- lookup (reverse k2) al
                   k4 <- lookup k3 al
                   s  <- lookup k4 al
                   Just ("You " ++ s)
```

That last line won't work, but `Monad` defines the operation `return`, which promotes a normal value into the Monad type.

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

```haskell
instance Monad Maybe where
    Nothing  >>= _ = Nothing
    (Just x) >>= k = k x
    return x       = Just x
```

so to fix that we use `return` instead of `Just`

> correct use:
```haskell
lookup4 :: String -> [(String, String)] -> Maybe String
lookup4 k1 al = do k2 <- lookup k1 al 
                   k3 <- lookup (reverse k2) al
                   k4 <- lookup k3 al
                   s  <- lookup k4 al
                   return ("You " ++ s)
```

Here is the final version that is the clearest because it focuses on the consequential charateristic of this algorithm.

Magically all the error handling is being relegated to the last `Maybe` in lookup4 type signature.

And what about the `>>` definition in `Monad`?

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

`>>` is a bind (`>>=`) variant that discards the result from the previous computation (the one followed by `.then()` in JS).

This is mainly used for put string functions that just return units `()`.

```haskell
hello :: IO ()
hello = putStr "Hello " >> putStrLn "World!"
```

Even if using `do` with put string, the oprator used will be `>>` and not `>>=`.

```haskell
hello :: IO ()
hello = do putStr "Hello "
           putStrLn "World!"
```
