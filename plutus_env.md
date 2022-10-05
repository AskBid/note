# Install Nix
make sure to install single user not multiuser as that gets complicated with the path
[Link Guide](https://nixos.wiki/wiki/Nix_Installation_Guide)

```bash
$ sudo install -d -m755 -o $(id -u) -g $(id -g) /nix
$ curl -L https://nixos.org/nix/install | sh
```

for $PATH:

```
$ source $HOME/.nix-profile/etc/profile.d/nix.sh
```
check installation `nix-env --version`

# Plutus App

```
$ git clone git@github.com:input-output-hk/plutus-apps.git
```

## Binary Cache

This is to make plutus built quicker by using IOHK cache.
Find where your `nix.conf` is, [link](https://nixos.org/manual/nix/stable/command-ref/conf-file.html).
In my case it was `$ nvim /etc/nix/nix.conf`. If you can't find it `find -iname nix.conf` then create it and add this lines to the file.

```
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

Run `nix-shell` inside plutus-app. It will take a long time if `nix.conf` was not set appropriately.

<a id="tag"></a>Check that the tag for `plutus-app` is the correct tag used for the project you are working on shows in `project/cabal.project` at "source repository package".
in `/plutus-app`

```bash
$ git checkout <tag-name>
```

# Run Plutus App

Once you are sure the `plutus-app` tag is the same one used from the plutus porject app `cabal.project` you can run `nix-shell` in the plutus-app folder and go in the project diretory (we cloned [gimbalabs pbl repo](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02)).

```bash
$ cabal clean; cabal update
$ cabal build
$ cabal repl
```

in `MyFirstPlutusScript.hs` there is the most basic validator script, it doesn't do anything.
This is imported in `MyFirstPlutusCompiler.hs` where it is compiled and saved as plutus core script ready to be put on chain.
Run `writeMyFirstValidatorScript` function in the repl to compile the basic validator.

```bash
Prelude Project01.MyFirstPlutusCompiler> writeMyFirstValidatorScript
Right ()
```

# Cardano Node

Download [executables](https://github.com/input-output-hk/cardano-node#executables) or install from [source](https://github.com/input-output-hk/cardano-node).
if a new machine probably better doing it from source as it will install all the packges and library required for cardano in general as Haskell.
Extract the executables folder in a directory (`~/cardano`) then make sure that folder is in your $PATH.
Make sure it works by checking `cardano-node --version; cardano-cli --version`.

<br>

## Upgrades

>If you already have `cardano-node` repo from previous built, you should be able torebuild with a new version with:
>```
>git fetch --all --recurse-submodules --tags
>git tag
>git checkout tags/<TAGGED VERSION>
>```
>`git describe --tags` to show current tag the repo is on.
>You should check this page for [latest instructions](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/install.md/#configuring-the-build-options).
>then:
>```
>cabal build all
>```
>You can now copy the executables in the location in your $PATH (usually ~/.local/bin/)

<br>


Download the [config, genesis and topology files](https://hydra.iohk.io/build/13695229/download/1/index.html). Set them up in the locations of your choice, then run the node and wait for it to sync.
There are different testnets running, at anytime may be better to switch to a certain new testnet and each of them may require a different version of the node to sync appropriately.
To find [other TESTNET config files](https://book.world.dev.cardano.org/environments.html) and [here is the faucet page](https://docs.cardano.org/cardano-testnet/tools/faucet) from where you can select different testnet environments. To make sure to have the [latest `cardano-node`](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.3)

```bash
$ cardano-node run \
--topology /home/marep/cardano/testnet-config/testnet-topology.json \
--database-path /home/marep/cardano/testnet/db \
--socket-path /home/marep/cardano/testnet/db/node.socket \
--host-addr 0.0.0.0 \
--port 3001 \
--config /home/marep/cardano/testnet-config/testnet-config.json
```

You can run cnode on a different machine and install sshd (open ssh) to access that machine by ssh with a tmux session. Remember to change the ssh port to something in the `/etc/ssh/sshd_config` (or similar) by uncommentin the `#port 22` line.
Once you can control the cardano-node on that machine for maintenance through ssh and tmux, you actually don't need to run anything else on that machine adn you can use the socket of that cnode from your other machine by running the following command.

```bash
ssh -nNT -L /tmp/forwarded.socket:/home/userx/cn/testnet/db/node.socket userx@8x.1xx.2xx.1x -p00000
```

A password will be asked and the terminal will remain idle.
At that point remember to set your `$CARDANO_NODE_SOCKET_PATH` to `/tmp/forwarded.socket` and you will be able to use `cardano-cli` on your local machine by accessing the node.socket of your remote machine.

[CSCLI](https://github.com/CardanoSharp/cscli) is a tool to use cardano-cli without the need to run a node.

You can now [build an address](https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/) with `cardano-cli`.

# Front End

Install `node`, `yarn`, `nvm`.
Try this fron-end interaction to Cardano template:
```
git clone https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-front-end-template
cd ppbl-front-end-template
yarn install
yarn dev
```
In a browser window open http://localhost:3000.

## Mesh Library

This template actually make use of the [Mesh library](https://mesh.martify.io/) which gives you an API to interact with wallets in the React env.
Following Mesh's guides you can recreate a template from scratch.

## [Koios](https://api.koios.rest/) REST API

Gets you information on the blockchain with simple REST API calls. Check `holdersByAssetID.txs` in gimbalabs front-end template.

```
https://testnet.koios.rest/api/v0/asset_address_list?_asset_policy=1309921891e459c7e9acb338d5dae18f98d1c2f55c1852cd5cf341f9&_asset_name=5050424c53756d6d657232303232
```

# [Metadata](https://developers.cardano.org/docs/transaction-metadata/how-to-create-a-metadata-transaction-cli#submit-to-blockchain)

You can add metadata on to the blockchain by simply sending a transaction that includes `--metadata-json-file`

```bash
cardano-cli transaction build-raw \
--tx-in 069bc710f11680205ff96dbd40affc4b319df0bee1b5093d2a739b1c2af9b6b8#0 \
--tx-out $(cat payment.addr)+0 \
--metadata-json-file metadata.json \
--fee 0 \
--out-file tx.draft 
```

```bash
cardano-cli transaction calculate-min-fee \
--tx-body-file tx.draft \
--tx-in-count 1 \
--tx-out-count 1 \
--witness-count 1 \
--byron-witness-count 0 \
--testnet-magic 1097911063 \
--protocol-params-file protocol.json
```

this will return the fee to be paid.

```bash
cardano-cli transaction build-raw \
--tx-in 069bc710f11680205ff96dbd40affc4b319df0bee1b5093d2a739b1c2af9b6b8#0 \
--tx-out $(cat payment.addr)+19825787 \
--metadata-json-file metadata.json \
--fee 174213 \
--out-file tx.draft 
```

```bash
cardano-cli transaction sign \             
--tx-body-file tx.draft \cardano-cli transaction submit \
--tx-file tx.signed \    
--testnet-magic 1097911063

--signing-key-file payment.skey \
--testnet-magic 1097911063 \
--out-file tx.signed 
```

## Retrive Metadata

The Metadata is resides with the transaction not with the Utxo. You need `cardano-db-sync` to query the metadata in transactions or you can query a `cardano-graphql` served from a cardano-db-sync node.
Here an example of a GraphQL query that looks for a metadata based on the key number used, it will return the items in chrnological order.

```graphql
{transactions(
      where: { metadata: { key: {_eq: "1337"} } }
      order_by: { includedAt: desc }
  ) {
      hash
      includedAt
      metadata {
          key
          value
      }
  }
}
```

# [Minting Native Assets with cardano-cli](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-02/docs/201-3.md)  
[Link Cardano Docs](https://developers.cardano.org/docs/native-tokens/minting)

## Cardano Scripts
Scripts are intended as a set of instructions, in our case instructions to the Cardano protocol.
In Gimbalabs courses there are 2 types of scripts **Native** and **Plutus** scripts.
**Native** can be written by hand and used directly in Cardano transactions.

## Native Script 

```bash
mkdir native-scripts
cd native-scripts

touch my-first-policy.script
```

Open the script and insert the following

```json
{
    "keyHash": "",
    "type": "sig"
}
```

You can use `cardano-cli` to generate a keyHash by using any Verification Key (aka Public Key). Basically you are using an hash to tell which private key should sign this script.
The script instruction `"type": "sig"` means that the instruction specifies a signature required by any transaction using the script. The `keyHash` defines which signature is required. Instead of putting the publicKey, we put an hash of it that we can generate from a public/verification key as follow.

```bash
cardano-cli address key-hash --payment-verification-key-file payment.vkey
```

Of course you'll need an exsting verification key.
i.e. for the following `payment.vkey`

```json
{
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": "58200daf6b11d22328aeebf5eb6212206a56c1e3a0ab083d8ca0c395394d57eb2fdf"
}
```

You'll get this keyHash `eee13a540ff63ee1a46c074f5bc1e2f1c324565a538532b746fd6c1d` using the `cardano-cli` command above.
You should now add your keyHash in your script.

```json
{
    "keyHash": "eee13a540ff63ee1a46c074f5bc1e2f1c324565a538532b746fd6c1d",
    "type": "sig"
}
```

<br>

You can now obtain your **policy ID**

```bash
cardano-cli transaction policyid --script-file my-first-policy.script
```

```bash
d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286
```

This creates a brand new token minting policy that can be used to mint tokens on Cardano.

<br>

Now build the first transaction to calculate our fee. But first we have to get the tokens names in **base16 encoded**.

```bash
$ echo -n "TokenName1"  | xxd -ps
> 546f6b656e4e616d6531
$ echo -n "RandomTokenName2" | xxd -ps
> 52616e646f6d546f6b656e4e616d6532
```

We will make 2 tokens just to show it doesn't need to be just one token from one policy (think at many NFTs made from the same policyID).

```bash
# Fee calculation tx

cardano-cli transaction build-raw \
--fee 300000 \
--tx-in 5e058b885fd3f07f906d65d628d1e5e48b75b502b5a310119023a7aad5582468#0 \
--tx-out addr_test1vrhwzwj5plmracdydsr57k7putcuxfzktffc2v4hgm7kc8gfs8qjk+10000000000+"25 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.546f6b656e4e616d6531 + 33 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.52616e646f6d546f6b656e4e616d6532" \
--mint "25 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.546f6b656e4e616d6531 + 33 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.52616e646f6d546f6b656e4e616d6532" \
--minting-script-file /home/marep/git/native-scripts/my-first-policy.script \
--out-file tx.raw

# with variables for better readability

cardano-cli transaction build-raw \
--fee $fee \
--tx-in $txhash#$txix \
--tx-out $address+$output+"$tokenamount $policyid.$tokenname1 + $tokenamount $policyid.$tokenname2" \
--mint "$tokenamount $policyid.$tokenname1 + $tokenamount $policyid.$tokenname2" \
--minting-script-file policy/policy.script \
--out-file tx.raw
```

> Why 3000000 for the fee? Transactions only used to calculate fees must still have a fee set, though it doesn't have to be exact. The calculated fee will be included the second time this transaction is built (i.e. the transaction to sign and submit). This first time, only the fee parameter length matters, so here we choose a maximum value

```bash
cardano-cli query protocol-parameters --out-file protocol.json --testnet-magic 2
```

```bash
cardano-cli transaction calculate-min-fee \
--tx-body-file tx.raw \
--tx-in-count 1 \
--tx-out-count 1 \
--witness-count 2 \
--testnet-magic 2 \
--protocol-params-file protocol.json
> 183629

expr 10000000000 - 183629
> 9999816371
```

```bash
cardano-cli transaction build-raw \
--fee $fee  \
--tx-in $txhash#$txix  \
--tx-out $address+$output+"$tokenamount $policyid.$tokenname1 + $tokenamount $policyid.$tokenname2" \
--mint "$tokenamount $policyid.$tokenname1 + $tokenamount $policyid.$tokenname2" \
--minting-script-file policy/policy.script \
--out-file matx.raw

# with variables for better readability

cardano-cli transaction build-raw \
--fee 183629 \
--tx-in 5e058b885fd3f07f906d65d628d1e5e48b75b502b5a310119023a7aad5582468#0  \
--tx-out addr_test1vrhwzwj5plmracdydsr57k7putcuxfzktffc2v4hgm7kc8gfs8qjk+9999816371+"25 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.546f6b656e4e616d6531 + 33 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.52616e646f6d546f6b656e4e616d6532" \
--mint "25 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.546f6b656e4e616d6531 + 33 d7d7f0d98149b56f7c6a884e2a263006977219b29accbd787f959286.52616e646f6d546f6b656e4e616d6532" \
--minting-script-file /home/marep/git/native-scripts/my-first-policy.script \
--out-file tx.raw
```

```bash
cardano-cli transaction sign \
--signing-key-file payment.skey \
<!-- --signing-key-file policy/policy.skey \ this will be required only if the vkey used for the script hash is different from the wallet you transact from-->
--testnet-magic 2 \
--tx-body-file tx.raw \
--out-file tx.signed
```

```bash
cardano-cli transaction submit --tx-file tx.signed --testnet-magic 2
```

You can now check you received the assets.

```bash
cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 2
```


## [Pluts Script](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-02/docs/201-4.md)

```bash
cd /project-02
mkdir output
```

go in `/plutus-app` check that the [tag](#tag) is the correct one.

```bash
nix-shell
```

Once nix-shell starts, go into the project folder `/project-02`.

```bash
cabal repl
Prelude Project02.PlutusMintingCompiler> writeMintingValidatorScript
```

Now this script is being created in `/output`.

```json
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "59073259072f01000032323232323232323232323232323233223232323222323253353232350020123333573466e... <many chars> ..."
}
```

As with the native script also with the plutus script we now use a similar command to create the policyID.

```bash
cd output
cardano-cli transaction policyid --script-file my-minting-script.plutus
> a10aa40d0ec3fd4c8fa33a2910fb27941ae8b7ad2a5b0c30816f7a20
```

Unlike with the native script with plutus script we need a redeemer. 
create a file called redeemer.json and paste the following into it:

```json
{"constructor":0,"fields":[]}
```

Redeemers are a big deal in Plutus, but this one is not. We just need a placeholder because a Plutus Minting Validator requires a Redeemer.

## Collateral UTXO

When a Plutus Script is used in a transaction, a "Collateral" UTXO must be specified. [Read about Collateral here](https://docs.cardano.org/plutus/collateral-mechanism).

Choose a UTXO that has approximately 5 ada in it, and that does not include any native assets. If you don't have a UTXO that meets this requirement send a UTXO to yourself that meets the requirements.

```bash
echo -n "PlutusMintToken"  | xxd -ps
> 506c757475734d696e74546f6b656e
```

```bash
cd output

cardano-cli transaction build \
--testnet-magic 1 \
--tx-in $TXIN \
--tx-in $TXIN2COLLATEAL \
--tx-out $MINTER+1500000+"$MINTAMOUNT $POLICYID.$TOKENNAME" \
--change-address $MINTER \
--mint "$MINTAMOUNT $POLICYID.$TOKENNAME" \
--mint-script-file $SCRIPTFILE \
--mint-redeemer-file $REDEEMERFILE \
--tx-in-collateral $COLLATERAL \
--protocol-params-file protocol.json \
--out-file mint-token-plutus.raw

cardano-cli transaction build \
--testnet-magic 1 \
--tx-in c447608608c8254acba9bdd5c91d72537f71be4d0b042c9d320bf3a124d470d1#0 \
--tx-out addr_test1vrhwzwj5plmracdydsr57k7putcuxfzktffc2v4hgm7kc8gfs8qjk+1500000+"37 a10aa40d0ec3fd4c8fa33a2910fb27941ae8b7ad2a5b0c30816f7a20.506c757475734d696e74546f6b656e" \
--change-address addr_test1vrhwzwj5plmracdydsr57k7putcuxfzktffc2v4hgm7kc8gfs8qjk \
--mint "37 a10aa40d0ec3fd4c8fa33a2910fb27941ae8b7ad2a5b0c30816f7a20.506c757475734d696e74546f6b656e" \
--mint-script-file my-minting-script.plutus \
--mint-redeemer-file redeemer.json \
--tx-in-collateral c447608608c8254acba9bdd5c91d72537f71be4d0b042c9d320bf3a124d470d1#1 \
--protocol-params-file protocol.json \
--out-file mint-token-plutus.raw
```

```bash
cardano-cli transaction sign \
--signing-key-file ~/cardano/pre-pod-wallet/payment.skey \
--testnet-magic 1 \
--tx-body-file mint-token-plutus.raw \
--out-file mint-token-plutus.signed

cardano-cli transaction submit \
--tx-file mint-token-plutus.signed \
--testnet-magic 1

```


# NFT

In terms of technology, there isn't much of a distinction between "regular" tokens/native assets and NFTs. This is due to the fact that both can be produced using the cardano node cli and are native assets.

Having alrady setup your payment.addr make a folder for the policy of your NFT.

```
mkdir policy
cd policy
```

```
cardano-cli address key-gen \
    --verification-key-file policy/policy.vkey \
    --signing-key-file policy/policy.skey
```

```
cardano-cli query protocol-parameters --mainnet --out-file protocol.json
```

Instead of only defining a single signature (as we did in the native asset minting guide), our script will ask for a slot number indicating the time after which no further minting or burning of the asset will be allowed.

```bash
cardano-cli address key-hash --payment-verification-key-file policy.vkey
> e6ae2c8edea3977a8184b8a6cc367c65ab6e2d260fe9f1477733060c
```

```bash
expr $(cardano-cli query tip --testnet-magic 1 | jq .slot) + 10000
>  6353183
```

Create a json file for the policy.script `nvim policy.script`.

```json
{
  "type": "all",
  "scripts":
  [
    {
      "type": "before",
      "slot": 6353183
    },
    {
      "type": "sig",
      "keyHash": "e6ae2c8edea3977a8184b8a6cc367c65ab6e2d260fe9f1477733060c"
    }
  ]
}
```

```
echo -n "MyFirstNFT"  | xxd -ps
```

Let's now build the transaction.

```bash
cardano-cli transaction build \
--mainnet \
--alonzo-era \
--tx-in $txhash#$txix \
--tx-out $address+$output+"$tokenamount $policyid.$tokenname" \
--change-address $address \
--mint="$tokenamount $policyid.$tokenname" \
--minting-script-file $script \
--metadata-json-file metadata.json  \
--invalid-hereafter $slotnumber \
--witness-override 2 \
--out-file matx.raw
```



```bash
cardano-cli transaction build \
--testnet-magic 1 \
--tx-in 10658f354b36a7eb4f7eaf31ca1309ace8d6d733cfafcdda16c25796b81e6f70#0 \
--tx-out addr_test1vrhwzwj5plmracdydsr57k7putcuxfzktffc2v4hgm7kc8gfs8qjk+2198569+"1 1ed4e644a009ea5928606461ad44dd204452abb2146a75767eeef250.4d7946697273744e4654" \
--change-address addr_test1vrhwzwj5plmracdydsr57k7putcuxfzktffc2v4hgm7kc8gfs8qjk \
--mint="1 1ed4e644a009ea5928606461ad44dd204452abb2146a75767eeef250.4d7946697273744e4654" \
--mint-script-file policy.script \
--metadata-json-file metadata.json \
--witness-override 2 \
--out-file tx.raw
```

```
Estimated transaction fee: Lovelace 185081
```

```
cardano-cli transaction sign  \
--signing-key-file ../payment.skey  \
--signing-key-file policy.skey  \
--mainnet --tx-body-file tx.raw  \
--out-file tx.signed
```

```
cardano-cli transaction submit --tx-file tx.signed --testnet-magic 1
```
```
cardano-cli query utxo --address $(cat ../payment.addr) --testnet-magic 1
```


