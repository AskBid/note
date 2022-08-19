### Install Nix
make sure to install single user not multiuser as that gets complicated with the path
[Link Guide](https://nixos.wiki/wiki/Nix_Installation_Guide)
```
$ sudo install -d -m755 -o $(id -u) -g $(id -g) /nix
$ curl -L https://nixos.org/nix/install | sh
```
for $PATH:
```
$ source $HOME/.nix-profile/etc/profile.d/nix.sh
```
check installation `nix-env --version`

### Plutus App
```
$ git clone git@github.com:input-output-hk/plutus-apps.git
```
###### Binary Cache
This is to make plutus built quicker by using IOHK cache.
Find where your `nix.conf` is, [link](https://nixos.org/manual/nix/stable/command-ref/conf-file.html).
In my case it was `$ nvim /etc/nix/nix.conf`. If you can't find it `find -iname nix.conf` then create it and add this lines to the file.
```
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

Run `nix-shell` inside plutus-app. It will take a long time if `nix.conf` was not set appropriately.

Check that the tag for `plutus-app` is the correct tag used for the project you are working on shows in `project/cabal.project` at "source repository package".
in `/plutus-app`
```
$ git checkout <tag-name>
```

### Run Plutus App
Once you are sure the `plutus-app` tag is the same one used from the plutus porject app `cabal.project` you can run `nix-shell` the plutus-app folder and go in the project diretory.
```
$ cabal clean; cabal update
$ cabal build
$ cabal repl
```
in `MyFirstPlutusScript.hs` there is the most basic validator script, it doesn't do anything.
This is imported in `MyFirstPlutusCompiler.hs` where it is compiled and saved as plutus core script ready to be put on chain.
Run `writeMyFirstValidatorScript` function in the repl to compile the basic validator.
```
Prelude Project01.MyFirstPlutusCompiler> writeMyFirstValidatorScript
Right ()
```


