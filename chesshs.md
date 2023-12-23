The `gameParse` function doesn't take the text to parse as an argument. Instead, it uses the `attoparsec` library's parsing combinators to parse the input incrementally as it's consumed by the parser. Specifically, `pgnParser`, which is defined as many `gameParse`, will parse the input as a list of `PGN` values by repeatedly applying `gameParse` to the remaining input until no more matches are found.

In other words, to parse a `PGN` file using this code, you would first read the contents of the file into a `ByteString` value and then pass it to the `pgnParser` function to obtain a list of `PGN` values.

To convert a `string` into a `ByteString` in Haskell, you can use the `pack` function from the `Data.ByteString.Char8` module. Here's an example:

```haskell

import Data.ByteString.Char8 (pack)

myString :: String
myString = "Hello, world!"

myByteString :: ByteString
myByteString = pack myString
```

In this example, myString is a String, and myByteString is a ByteString that contains the same sequence of bytes as myString.

# Modify Package

If you want to modify the source code of the `chesshs` package, you can do so by cloning the repository and making your changes. You can then `build` and `install` the modified package locally using `cabal`. Here are the steps you can follow:

Clone the chesshs repository:
    
```bash
git clone https://github.com/alpmestan/chesshs.git
```

Make your changes to the code.

Build the package:

```bash
cd chesshs
cabal build
```


Install the package:

```
cabal install
```

This will install the modified package in your local package database.

Alternatively, if you are using the `chesshs` package in a project managed by cabal, you can modify the package source code by adding a local dependency on the modified package in your `cabal.project` file. Here are the steps you can follow:

Clone the `chesshs` repository and make your changes as described above.

Add the following lines to your `cabal.project` file:

```vbnet
    packages:
      ./path/to/chesshs
```

Replace `./path/to/chesshs` with the path to the cloned chesshs repository.

`Run cabal build` or `cabal run` to build or run your project with the modified `chesshs` package.

Note that if you modify the chesshs package, you will need to rebuild and reinstall it every time you make changes to the code.