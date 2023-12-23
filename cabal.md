https://youtu.be/-DHEmrKhjCM

```bash
mkdir project
cd project
cabal init --interactive
```

you can compile custom modules at `cabla repl` by adding them to the `.cabal` file.

```bash
library
    exposed-modules:  MyLib
                    , NewCustomModule
```

but in reality the right way is to import them in the `MyLib` module that is compiled with `cabal repl`.

```haskell
import Module

import Module (name1, name2)

import Module hiding (name1, name2)

import qualified Module 
    Module.name         --only way to call

import Module as NewName
```

You can combine all the above

You can import Data Types too

```haskell
data DataType = A | B | C

import Module (DataType(..))

import Module (DataType(A,B))
```

root

```
/app
    Main.hs
/src
    /Bar
        Foo.hs
```

Foo.hs

```haskell
module Bar.Foo where
```

Main.hs

```haskell
import Bar.Foo
```

specify that only name1 and name2 are exported.

```haskell
module Foo (name1, name2) where
```

if you don't specify everything gets exported.


```haskel
cabal build project
cabal exec project
--or
cabal run project
```

## Long Intro

https://cabal.readthedocs.io/en/3.4/developing-packages.html

### Basics

to create a new project 

```
cabal init
```

Cabal file and same initial file is created.

`.cabal` file contains metadata that cabal need to compile the program or a library. And also other descriptions that can be useful if we want to upload our code to Hackage for example.
`CHANGELOG.md` will record all the changes brought to the cabal structure.

```
cabal initi -l MIT
```

will of course initiate the project iwth a license file filling the correct attributes in the `.cabal` file too.

```cabal
executable testProject
    main-is:          Main.hs,

    -- Modules included in this executable, other than Main.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:
    build-depends:
        base ^>=4.14.3.0,
        testProject

    hs-source-dirs:   app,
    default-language: Haskell2010
```

this is the juice of `.cabal`, we can see that in this case we don't have any dependencies but the base code of course.
we also tell cabal were the source files are located `/app`.

to build this code we of course use

```
cabal build testProject
```

it will compile the code using all the required dependencies.

```
cabal exec testProject
```

will execute the compiled code and for instance in a demo the terminal will output `Hello World!`

If you now bring changes to the code you have again to `cabal build` then `cabal exec`.
That's why we have a shortcut with `cabal run testProject`.

When you build a source code a new directory `dist-newstyle` will be created with the compiled files to be executed

### Libraries

that was a very basic program. but often our appications will be more complicated and we will require other packages to be run.

With  Haskell libraries are not other than other functions that are not defined in the main module but in other modules.

A way to create use that is to create a new directory `/src` with inside `Lib.hs` files for each modules we need to use for our program.

`Lib.hs`
```haskell
module Lib where

helloWorld :: IO ()
helloWorld = putStrLn "Hello World!"
```

`Main.hs`
```haskell
module Main where

import qualified Lib

main :: IO ()
main = Lib.helloWorld
```

But at this point we will see that our editor would highlight some errors because our `.cabal` doesn't know where to find the library modules.

That happens because we told `.cabal` that our source code directory is only `/app`, so we need to add `/src`.

```cabal
hs-source-dirs:   app, src
```

Not only that though, we also need to add the module.

```cabal
    -- Modules included in this executable, other than Main.
    other-modules: Lib     --this was hidden we need to use it, specifing Lib
```

Indeed usually the Main doesn't do anything but just call the real code present in other modules.

But sometime we are really not interested to have an executable and we just want to build a library to use somewhere else.
We can tell `.cabal` of this intention of ours by adding in the metadata the following:

```cabal
library
    exposed-modules: Lib

    build-depends: base ^>=4.14.3.0,
    hs-source-dirs:   src
    default-language: Haskell2010
```

> Only one library is allowed in cabal for whatever reason

notice we gave now the `/src` dir.

of course now we wouldn't have anymore any executable so there is no point in running this libray source.

Weat we do in this case is though to run `cabal repl` and load the library in it to play with.
REPL means Read Eval Print Loop.

```
cabal repl 
compiling (Lib) ( src/Lib.hs, interpreted)
*Lib>
```

Guess we still had our executable in `.cabal`. the `buils-depends:` and `default-language:` would be a repeat.
To avoid repetition we can use the `common` metadata.

```cabal
common use-a-name-for-common
    build-depends:    base ^>=4.14.3.0
    default-language: Haskell2010 

library
    import:           use-a-name-for-common
    exposed-modules:  Lib
    hs-source-dirs:   src

executable testProject
    import:           use-a-name-for-common
    main-is:          Main.hs,
    hs-source-dirs:   app, src
```

You can also add Ghci options in this `.cabal` metadata group.

```cabal
    ghc-options: -Wall
```

That's a ghci Warning All option.

You can run `cabal check` to check if everythig in the `.cabal` is in order.

```
cabal check
```

Could also just give you some advice rather than errors.

Perhaps you'll want multiple executable in our program, one for debugging and one for the normal version.

You create another Main.hs module calling it diffently off course ad add another metadata to `.cabal` wiht `executable differentName`.
Even though this different named executable is not called Main, you will still have to call the module Main `module Main where` otherwise when you `cabal run` it won't work.

But there is actaully a work around, and that is to use `ghc-options: -main-is AnotherMainName`

## Dependencies

For now our `.cabal` only depends on the base lib, but that's trivial.

if we `import Control.DeepSeq ( Force )` the editor will tell us that is missing because that's memeber of a hidden package.
What we need to do is to add `Control.DeepSeq` to our dependencies in our `.cabal` file.

so from 

```cabal
common common-all
    build-depends:    base ^>=4.14.3.0,
```

we would modify to

```cabal
common common-all
    build-depends:    
        base ^>=4.14.3.0,
        deepseq >= 1.4.4.0
```

here is specifing to have a version equal or higher than 1.4.4.0. could also use `==` and many more.

If you insert a packeage that is not already installed on your machine, but that you found on Hackage, when you compile the code, cabal will download that package for you.

So for instance we now want to write some test for our program and need QuickCheck, we will then proceed to insert it in our `.cabal` file.
We cancel deepseq because we don't really need it, was just an example.

```cabal
common common-all
    build-depends:    
        base ^>=4.14.3.0,
        QuickCheck
```

if we also add `cabal-test-quickcheck` we'll have a fast way to create tests. But that also needs `Cabal >= 3.0.1.0`.

Having now those dependencies we can add the metadata for the test suite

```cabal
Test-Suite my-test
    import:         common-all,
    type:           detailed-0.9,
    hs-source-dirs: src, test
    test-module:    Test
    other-modules:  Lib
```
> NOTICE the `import:` tag needs to be at the top.
> yes in this case the metadata tag is capital letters.

And here is how we write a test module:

```haskell
module Test ( tests ) where

import Distribution.TestSuite.QuickCheck ( Test, testProperty)
import qualified Lib

tests :: IO [ Test ]
tests = return [ 
                    testProperty "Lib.plus is the addition" someProperty,
                    testProperty "Always fails" false
               ]

someProperty :: Integer -> Integer -> Bool
someProperty a b = Lib.plus a b == a + b 
```

the test module exports a list of tests.

the above would test on this file

Lib.hs
```haskell
module Lib where

plus :: Integer -> Integer -> Integer
plus = ( + )
```

So how do we test now that we have the code to test and the tests code?

```
cabal test my-test
```

It will resolve dependencies, compilation, all that kind of stuff.. then will tell us if it has passed or not. there will also be a log in the directory `/test`.


## Stack

Stack is another package manager that intertwines with Cabal to offers more features as profiling.

## GHCup

use thi to upgrade GHC

## Plutarch

add a file `cabal.project` with the following content:

```
packages: .

source-repository-package
  type: git
  location: https://github.com/Plutonomicon/plutarch-plutus.git
  tag: 9b83892057f2aaaed76e3af6193ad1ae242244cc
```

The tag may change.

Then add the dependency in `project.cabal` file.

You should now be able to use `cabal repl` to use Plutarch 

> at time of writign you actually couldn't compile it when repling

this worked:

```
source-repository-package
  type: git
  location: https://github.com/Plutonomicon/plutarch-plutus.git
  tag: v1.3.0

repository cardano-haskell-packages
  url: https://input-output-hk.github.io/cardano-haskell-packages
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee

index-state: 2022-10-31T00:00:00Z
index-state: cardano-haskell-packages 2022-10-31T00:00:00Z

constraints:
  dependent-sum >= 0.7.1.0

package nothunks
  flags: +vector

packages: .
```


