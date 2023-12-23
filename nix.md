```haskell
module Main where

main :: IO ()
main = putStrLn "Hello World!"
```

```
$ nix-shell -p ghc
[nix-shell:~] runhaskell HelloWorld.hs
Hello World!
```

This would have worked in the normal way too..

Let's add a dependency

```
-- HelloWorld.hs
module Main where

import HaskellSay

main :: IO ()
main = haskellSay "Hello World!"
```

```
$ nix-shell -p ghc
[nix-shell:~] runhaskell HelloWorld.hs
```

gives error

```
HelloWorld.hs:2:1: error:
    Could not find module `HaskellSay'
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
2 | import HaskellSay
  | ^^^^^^^^^
```

```
$ nix-shell \
    -p "haskellPackages.ghcWithPackages (p: [p.haskell-say])" \
    --run "runhaskell HelloWorld.hs"
```

```
  ________________________________________________________
 /                                                        \
| Hello World!                                             |
 \____       _____________________________________________/
      \    /
       \  /
        \/
  _____   _____
  \    \  \    \
   \    \  \    \
    \    \  \    \
     \    \  \    \  \-----------|
      \    \  \    \  \          |
       \    \  \    \  \---------|
       /    /  /     \
      /    /  /       \  \-------|
     /    /  /    ^    \  \      |
    /    /  /    / \    \  \ ----|
   /    /  /    /   \    \
  /____/  /____/     \____\
```

You can also create a bash file:

```
#!/bin/bash
nix-shell \
    -p "haskellPackages.ghcWithPackages (p: [p.haskell-say])" \
    --run "runhaskell HelloWorld.hs"
```

> `$ chmod +x run-test.sh`

https://nixos.wiki/wiki/Nix_vs._Linux_Standard_Base



