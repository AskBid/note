# index

## [Read, List Comprension and fromListWith](#read-list-comprehension-and-fromlistwith) &nbsp;&nbsp;&nbsp;&nbsp;


```haskell
module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = 
  let exeptions year | (mod year 100 == 0) && (mod year 400 /= 0) = False
                     | otherwise = True
      leaped year = mod year 4 == 0
  in leaped year && exeptions year
```

```haskell
module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram sentence = all (`elem` map toLower sentence) allAlphabet

allAlphabet :: [Char]
allAlphabet = ['a'..'z'] -- ++ ['A'..'Z']

-- import Data.List (nub)

-- alphabetCount :: Int
-- alphabetCount =  length ['a'..'b']

-- isAlphabet :: Int -> Bool
-- isAlphabet n
--   | n >= alphabetCount = True
--   | otherwise          = False
```

```haskell
module Bob (responseFor) where
import           Data.Char
responseFor :: String -> String
responseFor input
    | null text = "Fine. Be that way!"
    | isShouting && isAsking = "Calm down, I know what I'm doing!"
    | isShouting = "Whoa, chill out!"
    | isAsking = "Sure."
    | otherwise = "Whatever."
  where
    text = filter (not . isSpace) input
    letters = filter isLetter text
    isShouting = all isUpper letters && letters /= ""
    isAsking = last text == '?'
```

```haskell
module CollatzConjecture (collatz) where
collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0 = Nothing
    | n == 1 = Just 0
    | even n = (+1) <$> collatz (n `div` 2)
    | otherwise = (+1) <$> collatz (3 * n + 1)
```

-----------------------------------------------------------------------------------------------------

```haskell
module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs
  | xs == ""              = Right ""
  | xs `contained` "CGTA" = Right (map (toNucleotide) xs)
  | otherwise             = Left (onlyError xs)

toNucleotide :: Char -> Char
toNucleotide 'C' = 'G'
toNucleotide 'G' = 'C'
toNucleotide 'T' = 'A'
toNucleotide 'A' = 'U'
toNucleotide  x  = x

contained :: Eq a => [a] -> [a] -> Bool
xs `contained` ys = all (`elem` ys) xs

onlyError :: [Char] -> Char
onlyError cs = head (filter (`notElem` "CTGA") cs)

-- notElem :: a -> [a] -> Bool
-- a `notElem` xs = not (a `elem` xs) 
```

Using `traverse` instead.

```haskell
module DNA (toRNA) where
  
toRNA :: String -> Either Char String
toRNA = traverse fromDNA
  where
    fromDNA :: Char -> Either Char Char
    fromDNA 'G' = pure 'C'
    fromDNA 'C' = pure 'G'
    fromDNA 'T' = pure 'A'
    fromDNA 'A' = pure 'U'
    fromDNA c = Left c
```

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
## Read, List Comprehension and fromListWith
[index](#index)

```haskell
import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | isValid xs = Right $ count xs
  | otherwise = Left xs
  where
    isValid = all (`elem` "ACGT")

count :: String -> Map Nucleotide Int
count xs = M.fromListWith (+) [(read [toUpper c], 1) | c <- xs]
```

The `read` function is a bit ambiguous when used by itself because it doesn't know what type you expect the result to be. `read "C"` In this case, it's trying to infer the type, and it's having trouble doing so.

You can resolve this issue by providing a type annotation to specify the expected type. In the context of your Nucleotide type, you can do the following:

```haskell
read "C" :: Nucleotide
```

`(+)` is used in the expression `M.fromListWith (+)` in conjunction with the `fromListWith` function from the `Data.Map` module. `fromListWith` is a function that takes a list of key-value pairs and creates a map, but if there are duplicate keys, it uses a combining function (in this case, `(+)`) to combine the values associated with those keys.

and here your miserable solution:
```haskell
import Data.Map (Map, fromList, (!), insert)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts "" = pure emptycount
nucleotideCounts str = func str emptycount

func :: String -> Map Nucleotide Int -> Either String (Map Nucleotide Int)
func (x:xs) m 
  | x == 'A' = func xs (increment (fromCharToNucl x) m)
  | x == 'C' = func xs (increment (fromCharToNucl x) m)
  | x == 'G' = func xs (increment (fromCharToNucl x) m)
  | x == 'T' = func xs (increment (fromCharToNucl x) m)
  | otherwise = Left (x:xs)
func "" m = Right m

countNucl :: Nucleotide -> String -> Int
countNucl n str = countElem (fromNuclToChar n) str

fromCharToNucl :: Char -> Nucleotide
fromCharToNucl 'A' = A
fromCharToNucl 'C' = C
fromCharToNucl 'G' = G
fromCharToNucl 'T' = T
fromCharToNucl _   = T

fromNuclToChar :: Nucleotide -> Char
fromNuclToChar A = 'A'
fromNuclToChar C = 'C'
fromNuclToChar G = 'G'
fromNuclToChar T = 'T'

emptycount :: Map Nucleotide Int
emptycount = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

increment :: Nucleotide -> Map Nucleotide Int -> Map Nucleotide Int
increment n m = insert n ((+1) $ m ! n) m

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)
```
-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
> Find multiples of an array of Int below a certain limit and sum them.

your attempt
```haskell
import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ concat $ map (getMultiples limit) factors 

getMultiples :: Integer -> Integer -> [Integer]
getMultiples limit number= [number, number*2 .. limit-1]
--tMultiples limit number= [number*1, number*2 .. limit-1]
```

A more elegant way with list comprehension
```haskell
import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub [x * y | x <- factors, y <- [1 .. limit], x * y < limit]
```
-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------

```haskell
import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n 
  | n < 65 && n > 0 = Just $ 2 ^ (n - 1)
  | otherwise       = Nothing

total :: Integer
total = sum $ fromJust <$> map square [1..64]
```

```sh
ghci> map square [1..64]
[Just 1,Just 2,Just 4,Just 8,Just 16,Just 32, ...
```

```sh
ghci> fromJust <?> map square [1..64]
[1, 2, 4, 8, 16, 32, ...
```

Funny enough even using `sum` works because its type takes a fodable type (an array intentionally) and returns the contained as a sum.

```sh
ghci> :t sum
sum :: (Foldable t, Num a) => t a -> a

ghci> sum <?> map square [1..64]
[1, 2, 4, 8, 16, 32, ...
```

here is another option:

```haskell
total :: Integer
total = sum $ mapMaybe square [1..64]
```

```sh
ghci> :t mapMaybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
```

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
## Acronim w/ `foldl`

```
"Fai _un_ Acronimo please"
"FUAP"

"Ma anche-cosi"
"MAC"
```

```haskell
import Data.Char

abbreviate :: String -> String
abbreviate = map (toUpper . head) . words . wordify

wordify :: String -> String
wordify (x : y : xs)
  | isLower x && isUpper y = x : ' ' : y : wordify xs
wordify ('_' : xs) = ' ' : xs
wordify ('-' : xs) = ' ' : xs
wordify (x : xs) = x : wordify xs
wordify [] = []
```

```haskell
import Data.Char

keepChar :: (Char, String) -> Char -> (Char, String)
keepChar (l, xs) c
  | l == '\'' && c == 's'           = (c, xs)
  | not (isLetter l) && isLetter c  = (c, xs ++ [toUpper c])
  | isLower l && isUpper c          = (c, xs ++ [c])
  | otherwise                       = (c, xs)

abbreviate :: String -> String
abbreviate = snd . foldl keepChar (' ', [])
--foldl :: Foldable t =>(b->a->b) ->  b  ->  t a      -> b
```

`keepChar` above goes as the whole first agument of the `foldl` (`(b -> a -> b)`), while `(' ', [])` becomes the accumulator (second argument of the `foldl`).

my version later done
```haskell
abbreviate :: String -> String
abbreviate xs = reverse . snd $ foldl patrol (' ', "") xs

patrol :: (Char, String) -> Char -> (Char, String)
patrol (prevc, xs) c
  | prevc == ' ' && isLetter c = (c, (toUpper c):xs)
  | prevc == '_' && isLetter c = (c, (toUpper c):xs)
  | prevc == '-' && isLetter c = (c, (toUpper c):xs)
  | isLower prevc && isUpper c = (c, c:xs)
  | otherwise                  = (c, xs)
```

notice the `fodl` and `foldr` differences:

```haskell
ghci> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
ghci> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

ghci> abbreviateWithFoldl "The Road Not Taken"
"TRNT"
ghci> abbreviateWithFoldr "The Road Not Taken"
"NTDE"
```

```haskell
import Data.Char

abbreviate :: String -> String
abbreviate = map (toUpper . head) . splitString . removeUnderscore

removeUnderscore :: String -> String
removeUnderscore = filter (/= '_')

splitString :: String -> [String]
splitString = map reverse . reverse . foldl reducer [[]]

reducer :: [String] -> Char -> [String]
reducer (h:hs) x
  | isDelimiter x = if null h then h:hs else []:(h:hs)
  | isUpper x = if all isUpper h then (x:h):hs else [x]:(h:hs)
  | otherwise = (x:h):hs

isDelimiter :: Char -> Bool
isDelimiter x
  | x==' ' || x=='-' = True
  | otherwise = False
```
-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------

## Anagram using `\\`

```haskell
import Data.Char (toLower)
import Data.List ((\\))

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagram xs) xss

isAnagram :: String -> String -> Bool
isAnagram word tword
  | wL == twL = False
  | sameChars = True
  | otherwise = False 
  where wL  = toLower <$> word
        twL = toLower <$> tword
        sameChars = (null $ (\\) wL twL) && (null $ (\\) twL wL)
isAnagram _ _ = False
```

Interesting to see how `\\` is defined [docs link](https://hackage.haskell.org/package/base-4.19.0.0/docs/src/Data.OldList.html#local-6989586621679699363):
```haskell
(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) =  foldl (flip delete)
```
-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
## Clock w/ Record Syntax

```haskell
data Clock = Clock {
  h :: Int,
  m ::Int
} deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minutes = Clock (rollHours hour minutes) (mod minutes 60)

toString :: Clock -> String
toString clock = (showDigit $ h clock) ++ ":" ++ (showDigit $ m clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = fromHourMin (hour + (h clock)) (min + (m clock)) 

showDigit :: Int -> String
showDigit d 
  | d < 10    = "0" ++ show d
  | otherwise = show d

rollHours :: Int -> Int -> Int
rollHours h m 
  | m > 59 || m < 0 = (h + (m `div` 60)) `mod` 24
  | otherwise       = h `mod` 24
```
-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------

## Number List Comprehension

yours:
```haskell
data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify 1           = Just Deficient
classify n
  | n < 1            = Nothing
  | (aliquot n) == n = pure Perfect
  | (aliquot n) < n  = pure Deficient
  | (aliquot n) > n  = pure Abundant
  | otherwise        = Nothing

aliquot :: Int -> Int
aliquot n = (+1) . sum $ filter (divides n) [2..n-1]

divides :: Int -> Int -> Bool
divides n d 
  | n `mod` d == 0 = True
  | otherwise      = False
```

smarter List Comprehension
```haskell
data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)
classify :: Int -> Maybe Classification
classify n
  | n <= 0      = Nothing
  | aliquot > n = Just Abundant
  | aliquot < n = Just Deficient
  | otherwise   = Just Perfect
  where aliquot = sum [x | x <- [1..(n `div` 2)], n `mod` x == 0]
```

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------

yours:
```haskell
encode :: String -> String
encode "" = ""
encode xs = unwords . transpose $ encodeSquare (toLower <$> xs)

squareit :: Int -> Int
squareit n = ceil
  where root = sqrt (fromIntegral n) :: Double
        ceil = ceiling root

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = as : chunkList n bs 
  where (as,bs) = splitAt n xs
        
encodeSquare :: String -> [String]
encodeSquare str = chunkList sqr $ addSpaces spaces str'
  where str' = concat . words $ (filter (isNumberLetter) str)
        sqr  = squareit $ length str'
        spaces = spacesToAdd str' sqr

isNumberLetter :: Char -> Bool
isNumberLetter c 
  | isLetter c = True
  | isNumber c = True
  | otherwise  = False

addSpaces :: Int -> String -> String
addSpaces 0 str = str
addSpaces n str = addSpaces (n-1) $ str ++ " "

spacesToAdd :: String -> Int -> Int
spacesToAdd str sqr
  | mod' <= 0 = 0
  | otherwise = sqr - mod'
  where  mod' = (length str) `mod` sqr
```

others
```haskell

encode :: String -> String
encode xs = unwords . transpose . fmap pad . chunksOf cols $ fmap toLower xs'
  where
    xs' :: String
    xs' = filter (\c -> isAlpha c || isDigit c) xs

    rows :: Int
    rows = floor . sqrt' $ length xs'

    cols :: Int
    cols = rows + bool 0 1 (rows * rows < length xs')

    pad :: String -> String
    pad s = s <> replicate (cols - length s) ' '

sqrt' :: Int -> Float
sqrt' = sqrt . fromIntegral


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as : chunksOf n bs 
  where (as,bs) = splitAt n xs
```

## `(<>)` is like `concat` 

```haskell
import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)
import Data.List.Split (chunksOf)

rightPad :: Int -> String -> String
rightPad n x = take n $ x ++ repeat ' '

encode :: String -> String
encode xs =
    let
        sanitized = map toLower . filter isAlphaNum $ xs
        square = sqrt @Double . fromIntegral . length $ sanitized
        c = ceiling square
        r = round square
     in
        unwords . transpose . chunksOf c . rightPad (c * r) $ sanitized
```

--------------------------------------
--------------------------------------
my solution:
```haskell
import Data.Char (isNumber, digitToInt)

isValid :: String -> Bool
isValid []  = False
isValid " 0"= False
isValid [_] = False
isValid cs 
  | zeros     = True
  | otherwise = divisible $ sum (doubleEverySnd $ reverse cs)
  where zeros = length (filter (=='0') cs) > 1


doubleEverySnd :: [Char] -> [Int]
doubleEverySnd cs = [if even i then calcNum x else x | (x, i) <- zip (map digitToInt $ filter isNumber cs) [1..]]

calcNum :: Int -> Int
calcNum n 
 | n > 4     = (n * 2) - 9
 | otherwise = n * 2 

divisible :: Int -> Bool
divisible n 
  | mod' == 0 = True 
  | otherwise = False
  where mod' = n `mod` 10 
```

interesting solution:
```haskell
import Text.Read (readMaybe)

digits :: String -> Maybe [Integer]
digits = mapM (readMaybe . (: [])) . filter (/= ' ')

isValidNumber :: Maybe [Integer] -> Bool
isValidNumber Nothing = False
isValidNumber (Just digits) = length digits > 1 && mod adjustedSum 10 == 0
  where
    indexedDigits = flip zip [1 ..] $ reverse digits
    doubleEven (d, i)
      | d == 9 = 9
      | even i = mod (d * 2) 9
      | otherwise = d
    adjustedSum = sum (map doubleEven indexedDigits)

isValid :: String -> Bool
isValid = isValidNumber . digits
```

nice way to double only other second digit `doubleEverySecond`
```haskell
isValid :: String -> Bool
isValid n
  | length stripped <= 1 = False
  | otherwise = isDivisibleByTen $ calculateLuhnNumber $ toIntList stripped
  where
    stripped = removeSpaces n
    
charToInteger :: Char -> Integer
charToInteger c = read [c]

toIntList :: String -> [Integer]
toIntList cs = map charToInteger $ reverse cs

removeSpaces :: String -> String
removeSpaces cs = filter (/=' ') cs

calculateLuhnNumber :: [Integer] -> Integer
calculateLuhnNumber xs = sum (map subtractIfNecessary $ doubleEverySecond xs)

doubleEverySecond [x] = [x]
doubleEverySecond [] = []
doubleEverySecond (x:y:xs) = (x:(y*2):(doubleEverySecond xs))

subtractIfNecessary x
  | x > 9 = x - 9
  | otherwise = x

isDivisibleByTen x = x `mod` 10 == 0
```

short variant:
```haskell
import Data.Char

isValid :: String -> Bool
isValid xs = length digits > 1 && luhnSum (reverse (map digitToInt digits)) `mod` 10 == 0
  where 
    digits = filter isDigit xs
    luhnSum [] = 0
    luhnSum [x] = x
    luhnSum (x:y:xs) = x + double y + luhnSum xs 
      where 
        double d = if d * 2 >= 10 then d * 2 - 9 else d * 2
```

```haskell
import Data.Char (digitToInt, isDigit)
import Data.List (partition)

isValid :: String -> Bool
isValid s = (length onlyDigits > 1) && (mod (sum xs + sum ys) 10 == 0)
  where
    xs = map ((\n -> if n < 5 then n * 2 else n * 2 - 9) . snd) $ fst xys
    ys = map snd $ snd xys
    xys = partition (even . fst) . zip ([1 ..] :: [Int]) . reverse . map digitToInt $ onlyDigits
    onlyDigits = filter isDigit s
```

```haskell
isValid :: String -> Bool
isValid raw = length digits > 1 && luhn `mod` 10 == 0
  where
    digits = map (\x -> read [x] :: Int) $ filter (/= ' ') raw
    luhn = sum $ zipWith tra (reverse digits) (cycle [1, 2])
    tra a b = if a * b < 10
              then a * b
              else (a * b) - 9
```

------------------------------------------------------------------------------
------------------------------------------------------------------------------
# Primes and func definition inside `where`

my solution:
```haskell
nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = pure $ cullMulti n 2 1 [2..]

cullMulti :: Int -> Int -> Int -> [Int] -> Integer
cullMulti _ _ _ [] = error "prime candiadate array came empty"
cullMulti n l c arr 
  | c < n   = cullMulti n h (c + 1) (h : t) 
  | otherwise = toInteger l
  where (h : t) = filter (divisible l) arr	

divisible :: Int -> Int -> Bool
divisible n x = x `mod` n /= 0
```

definition inside where solution:
```haskell
nth :: Int -> Maybe Integer
nth n 
  | n > 0 = Just $ primes !! (n - 1)
  | otherwise = Nothing

primes :: [Integer]
primes = sieve [2..]
  where
    sieve [] = []
    sieve (p:ps) = p : sieve [x | x <- ps, mod x p /= 0]
```

```haskell
nth :: Int -> Maybe Integer
nth n 
    | n < 1 = Nothing
    | n == 1 = Just 2
    | otherwise = Just $ filter isPrime [3, 5 .. ] !! (n - 2)
    where 
        isPrime :: Integer -> Bool
        isPrime i = all (\x -> rem i x > 0) [3 .. (floor.sqrt) (fromIntegral i)]
```

```haskell
nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just (primes !! (n - 1) )
  where primes = sieve [2..]
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
```
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
# Mines sweeper - List Comprehension

```haskell
annotate :: [[Char]] -> [[Char]]
annotate board = [[char y x | x <- [0..w]] | y <- [0..h]]
  where
    h = length board - 1
    w = length (head board) - 1
    isMine y x = (board !! y) !! x == '*'
    char y x
      | isMine y x = '*'
      | otherwise = case minesAround y x of
            0 -> ' '
            n -> head $ show n
    minesAround y x = length [(a, b) | a <- [max 0 (x - 1) .. min w (x + 1)],
                                       b <- [max 0 (y - 1) .. min h (y + 1)],
                                       isMine b a]
```
```haskell
h = length ["   ", " * ", "   "] - 1
h
-- 2
w = length (head ["   ", " * ", "   "]) - 1
w
-- 2

-- ["   "
-- ," * "
-- ,"   "]

[y | y <- [0..2]]
-- [0,1,2]

[x | x <- [0..2]]
-- [0,1,2]

[(x,'y') | x <- [0..2]]
-- [(0,'y'),(1,'y'),(2,'y')]

[[(x, y) | x <- [0..2]] | y <- [0..2]]

[[(0,0),(1,0),(2,0)]
,[(0,1),(1,1),(2,1)]
,[(0,2),(1,2),(2,2)]]

annotate board = [[char y x | x <- [0..w]] | y <- [0..h]]

char 0 0
char 1 0
char 2 0 
char 0 1
char 1 1
char 2 1
char 0 2
char 1 2
char 2 2
```

If `char 1 1` is a mine we will return a mine `*`.

otherwise we do a `char 0 1` is not a mine, `minesAround 1 1` will be run, which should return `1`

But to better understand what's going on we should first make a simpler version of `minesAround`, `spacesAround`

```haskell
spacesAround :: Int -> Int -> Int
spacesAround y x = length [(a, b) | a <- [max 0 (x - 1) .. min w' (x + 1)],
                                    b <- [max 0 (y - 1) .. min h' (y + 1)]]
```

In this case given coordinates `spaceAround` returns all the count of all spaces adjacent to the space of the coordinates.. and in reality it counts the space itself. That's because the `isMine b a` row escludes the spaces, and if the selected space is a mine, it won't evn start `minesAround`.

how does it work? given an index, it finds all the indexes from 1 above to 1 below, then the same for all colums, but keeping into account the borders thanks to the `max` and `min` function.

i.e. for `1 0` it would find `[0,1,2]` for `y` and `[0,1]` for `x`, that gives shape to an aray from list comprehension such as:

`(a, b)`

`(x, y)`

`(0,0) (0,1) (0,2)` 

`(1,0) (1,1) (1,2)` 

result would then be `[(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]`, which then gets counted to give `6`, which is all the spaces adjacent to cell `1, 0` including itselg (for `spacesAround`).

--------------------------------------------------------------
--------------------------------------------------------------
# Phone Number and the whole`@`(pie:ces) pattern

```haskell
import Data.Char (isDigit)

number :: String -> Maybe String
number xs = if length s == 10 && head s > '1' && s !! 3 > '1' then Just s else Nothing
  where
    s = dropWhile (== '1') $ filter isDigit xs
```

```haskell
import Data.Char 

number :: String -> Maybe String
number = check . dropCountryCode . filter isDigit
  where
    dropCountryCode xs = if head xs == '1' then tail xs else xs 
    check phone@[a, _, _, b, _, _, _, _, _, _] 
      | a > '1' && b > '1' = Just phone
    check _ = Nothing
```



	

	