# Low-Level, Untyped Validation Scripts

```
cabal repl
```

```haskell
import Plutus.V2.Ledger.Api

:i Data
```

```haskell
type Data :: *
data Data
  = Constr Integer [Data]
  | Map [(Data, Data)]
  | List [Data]
  | I Integer
  | B Data.ByteString.Internal.ByteString
```

```haskell
I 42
```

```
:set -XOverloadedStrings
```

Haskell only works with list of characters, but with this `:set` extension we can work with other string types as well in repl.

```haskell
B "Haskell"
```

```haskell
Map [(I 42, B "Haskell"), (List[I 0], I 1000)] -- ::Data
```

week02 code `Gift.hs`

```haskell
-- This validator always succeeds
--                    Datum         Redeemer     ScriptContext
mkGiftValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGiftValidator _ _ _ = ()
{-# INLINABLE mkGiftValidator #-}
```

Returning a unit `()` (void in Haskell) is strange for Haskell, but in this case we are only interested in the side effects of the Validator.

week02 code `Gift.hs`

```haskell
validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])
```

this is an Haskell template advanced scripting code that in practice simply converts our Haskell Validator into Plutus Core.

`PlutusTx.compile` takes not a function, but a piece of source code, which is wht the Oxford Brackets provide given a function `[||`.

What `compile` gives us is a complex syntax representing Plutus Core, but it will be the source code beaiscally, which we now need to reconvert in a function. The `$$` does that transformation of the syntax into function which then `PlutusV2.mkValidatorScript` turns into an actual validator.

`INLINABLE` make it so that when you call `mkGiftValidator` is like reqriting by hand the code representing that function. So everything that will be compiled to Plutus Core will need this inlinable Pragma.

Depending on how we want to use the validator it now needs to be serialised to be used for instance from the `cardano-cli`.

> serialisation: the process of translating a data structure or object state into a format that can be stored

that what this part of the code does, using other library that manipualte bytestring and other serialisation things.

week02 code `Gift.hs`

```haskell
saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/gift.plutus" validator
```

```
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "49480100002221200101"
}
```

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}                 -- <==
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Burn where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile)
import           PlutusTx.Prelude     (traceError)  -- <==
import           Prelude              (IO)          -- <==
import           Utilities            (writeValidatorToFile)
```

We don't implicitly import `Prelude` because we ned the one from `PlutusTx.Prelude`. The reason we need that is mainly because we need fucntions that can be inlinable, so that they can be templated and transalated in Plutus Core, and the Prelude was not written with inlinalble capabilites in mind. `PlutusTx.Prelude` is like `Prelude` (only few things missing) but it is INLINABlE (bc rewritten from IOG team).

# Off Chain

Build Script Address. Send locking ADA into script address.

```sh
#!/bin/bash

# Build gift address 
cardano-cli address build \
    --payment-script-file "$assets/gift.plutus" \
    --testnet-magic 2 \
    --out-file "$assets/gift.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/gift.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$assets/unit.json" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$body"
    
# Sign the transaction

# Submit the transaction
```

Notice we used `inline-datum` and not an hash, that will allow us in the spending transaction below to not to use the datum but only say `datum-present`. The Datum hash and the datum itself will be embedded and visible in the locked UTxO inside the contract address, accessible on cardano scan through the transaction hash.

Redeemer is only required when spending the contract UTxO not when locking it or giving it to the contract.

spending contract UTxO:

```sh
# Query the protocol parameters \

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/unit.json" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --protocol-params-file "$pp" \
    --out-file "$body"
    
# Sign the transaction

# Submit the transaction
```

We need to add the script now that we are spending a contract UTxO (or we can just reference it), and the redeemer (is an empty one because in teh gift contract doesn't really matter can be empty, but we still need to put one), and the protocol parameters, and a collateral (always returned unless malicious) and a datum but not in this case because we inlined it in the locking Tx as well.

if we use the third option in our locking Tx for the datum

```sh
--tx-out-datum-hash 
--tx-out-datum-embed-cbor-file
--tx-out-inline-datum-cbor-file
```

We can use the `--tx-in-inline-datum-present` option when we spend the contract UTxO 

```
--tx-in-datum-cbor-file
--tx-in-datum-file
--tx-in-datum-value
--tx-in-inline-datum-present
```

> **remember that a contract UTxO without any Datum can never be spent. It will be locked forever.**

# (High-Level, Typed Validation Scripts)[https://youtu.be/GT8OjOzsOb4]

Here a contract where we care of what the redeemer is to validate a spending transaction of a contract UTxO.

```haskell
-- This validator succeeds only if the redeemer is 42
--                  Datum         Redeemer     ScriptContext
mk42Validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mk42Validator _ r _
    | r == Builtins.mkI 42 = ()
    | otherwise            = traceError "expected 42"
{-# INLINABLE mk42Validator #-}
```

```haskell
Prelude> import PlutusTx.Builtins
Prelude PlutusTx.Builtins> mkI 42
I 42
Prelude PlutusTx.Builtins> :t mkI
mkI :: Integer -> BuiltinData
```

In theory we could write all of our Validators like this where all of the arguments are `BuiltinData` (perhaps only functions - as they are first citiziens in haskell - wouldn't be possible, but any other non operational data yes).

But if we use the same type for everything it defies the scope to have a typed language that can underline errors during compilation. That's why is helpful to have typed data that follow the logic of our contract.

```haskell
-- This validator succeeds only if the redeemer is 42
--              Datum  Redeemer        ScriptContext
mk42Validator :: () -> Integer -> PlutusV2.ScriptContext -> Bool
mk42Validator _ r _ = traceIfFalse "expected 42" $ r == 42
{-# INLINABLE mk42Validator #-}
```

The problem is that now the Haskell Template that converted our Validator to Plutus core won't work anymore

```haskell
validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mk42Validator ||])
```

```error
• Couldn't match type ‘()’ with ‘PlutusV2.BuiltinData’
  Expected type: template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Q
        (template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.TExp
        (PlutusTx.Code.CompiledCode (PlutusV2.BuiltinData -> PlutusV2.BuiltinData -> PlutusV2.BuiltinData -> ())))
    Actual type: th-compat-0.1.4:Language.Haskell.TH.Syntax.Compat.SpliceQ
        (PlutusTx.Code.CompiledCode (() -> Integer -> PlutusV2.ScriptContext -> Bool))
• In the expression: compile [|| mk42Validator ||]
  In the Template Haskell splice $$(compile [|| mk42Validator ||])
  In the first argument of ‘PlutusV2.mkValidatorScript’, namely
    ‘$$(compile [|| mk42Validator ||])’typecheck
```

To solve this error is enough to add the `wrap` helper imported from the Utilities module supplied by PPP.

```haskell
validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrap mk42Validator ||])
```

```haskell
-- module Utilities.PlutusTx
{-# INLINABLE wrap #-}
-- wrap :: forall a b.( UnsafeFromData a, UnsafeFromData b) => 
wrap :: (a -> b -> ScriptContext -> Bool) -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrap f a b ctx = check $ f (unsafeFromBuiltinData a) (unsafeFromBuiltinData b) (unsafeFromBuiltinData ctx)
```

```haskell
> import Plutus.V2.Ledger.Api
> :i UnsafeFromData 
type UnsafeFromData :: * -> Constraint
class UnsafeFromData a where
  unsafeFromBuiltinData :: BuiltinData -> a
  {-# MINIMAL unsafeFromBuiltinData #-}
-- ...
instance UnsafeFromData Bool
  -- Defined in `plutus-tx-1.0.0.0:PlutusTx.IsData.Instances'
-- ...
instance UnsafeFromData ()
  -- Defined in `plutus-tx-1.0.0.0:PlutusTx.IsData.Instances'
-- ...
instance UnsafeFromData Integer
  -- Defined in `PlutusTx.IsData.Class'
-- ...
instance UnsafeFromData ScriptContext
  -- Defined in `Plutus.V2.Ledger.Contexts'
-- ..
```

> Notice that above we have all the instances that some types (`()`, `Bool`, `Integer`, `ScriptContext`) are instantiating the classtype `UnsafeFromData`, and not the opposite.

`UnsafeFromData` is the type contraint we use in the `wrap` function (commented out above). In the `class` definition we can see that UnsafeFromData classtype required a function `unsafeFromBuiltinData` which is indeed the one that we are using in `wrap`.

What `unsafeFromBuiltinData` does is to take a `BuiltinData` and convert it in the parametrised type of choice (which instantiated `UnsafeFromData`).

But how can `unsafeFromBuiltinData` understand which type we want the `BuiltinData` to transform into? We have to specify the type `:: Type`.

```haskell
-- > import PlutusTx.Builtins
mkI 42
-- I 42
unsafeFromBuiltinData $ mkI 42
-- error
unsafeFromBuiltinData $ mkI 42 :: Integer
-- 42

unsafeFromBuiltinData $ mkB "Haskell" :: Integer
-- <interactive>:20:48: error:
--    lexical error in string/character literal at end of input
-- Prelude PlutusTx.Builtins Plutus.V2.Ledger.Api Gift> unsafeFromBuiltinData $ mkB "Haskell" :: Integer
-- *** Exception: not an I
unsafeFromBuiltinData $ mkB "Haskell" :: BuiltinByteString
-- "Haskell"
```

So in this typed version of the Validator if someone were to send a transaction with a `String` rather than an `Integer`, the Transaction would fail. So it would be a much safer version.

But there would be a perfomance hit. Here is the two scripts, the untyped one and the typed one.

```
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "58385836010000322225335333573466ebc008dd4240a82440042440022008264c649319ab9c49010b6578706563746564203432000041200101"
}
```

```
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "5907ac5907a901000032323322323232323232323232323233223232323232222323253353232325335333573466e1c009205401c01b101c13357389210b65787065637465642034320001b3333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd405c060d5d0a80619a80b80c1aba1500b33501701935742a014666aa036eb94068d5d0a804999aa80dbae501a35742a01066a02e0446ae85401cccd5406c08dd69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40b5d69aba15002302e357426ae8940088c98c80c0cd5ce01981801709aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a816bad35742a004605c6ae84d5d1280111931901819ab9c03303002e135573ca00226ea8004d5d09aba2500223263202c33573805e05805426aae7940044dd50009aba1500533501775c6ae854010ccd5406c07c8004d5d0a801999aa80dbae200135742a00460426ae84d5d1280111931901419ab9c02b028026135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860226ae84d5d1280211931900d19ab9c01d01a018375a00a6666ae68cdc39aab9d375400a9000100c11931900c19ab9c01b018016101713263201733573892010350543500017135573ca00226ea800448c88c008dd6000990009aa80b111999aab9f0012500a233500930043574200460066ae880080508c8c8cccd5cd19b8735573aa004900011991091980080180118061aba150023005357426ae8940088c98c8050cd5ce00b80a00909aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500f014357426ae8940088c98c8064cd5ce00e00c80b89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403666ae7007806c06406005c4d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201533573803002a02626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355013223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301213574200222440042442446600200800624464646666ae68cdc3a800a40004642446004006600a6ae84d55cf280191999ab9a3370ea0049001109100091931900819ab9c01301000e00d135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01301000e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00f00c00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00680500409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00b00980880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae7003c0300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801801200e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7003402802001c0184d55cea80089baa0012323333573466e1d40052002200723333573466e1d40092000200723263200633573801200c00800626aae74dd5000a4c240022440042440029210350543100112323001001223300330020020011"
}
```

### Custom Data Type

We have seen that there are a lot of predifined instances for `UnsafeFromData`, but what if you have Custom Type?

If you have a very specific business you want to write a contract for, you will have a very specific type as well.

That means we would have to write how to convert `BuiltinData` to that specific Custom Type through the `unsafeFromBuiltinData` classtype/interface.

```haskell
newtype MySillyRedeemer = MkMySillyRedeemer Integer
```

> `newtype` is like `data`, is not like the synonim `type`.


Luckily we don't have to actually write our own custom type `unsafeFromBuiltinData`. Haskell Templates come once again to our support.

```haskell
PlutusTx.unstableMakeIsData ''MySillyRedeemer
```

> there is also a `stableMakeIsData` which gives more control over how the data is serialised, but also requires more instructions on how to do so.

`''` this will insert in the surce code the instance of the data that follows.

Compiling and asking for info we can see that `unstableMakeIsData` adds something different than just `UnsafeFromData`. It uses a `FromData` that adds a `Maybe a` output so if we give a ByteString rather than an Integer as in teh example before it won't give an error but will return a `Nothing`.

```haskell
:i MySillyRedeemer
type MySillyRedeemer :: *
newtype MySillyRedeemer = MkMySillyRedeemer Integer
        -- Defined in `CustomTypes'
instance FromData MySillyRedeemer -- Defined in `CustomTypes'
instance ToData MySillyRedeemer -- Defined in `CustomTypes'
instance UnsafeFromData MySillyRedeemer -- Defined in `CustomTypes'
```

```haskell
:i FromData
type FromData :: * -> Constraint
class FromData a where
  fromBuiltinData :: BuiltinData -> Maybe a
  {-# MINIMAL fromBuiltinData #-}

:i UnsafeFromData
type UnsafeFromData :: * -> Constraint
class UnsafeFromData a where
  unsafeFromBuiltinData :: BuiltinData -> a
  {-# MINIMAL unsafeFromBuiltinData #-}
```

At this point we can use our own types in the validator.

```haskell
-- This validator succeeds only if the redeemer is `MkMySillyRedeemer 42`
--              Datum     Redeemer            ScriptContext
mkCTValidator :: () -> MySillyRedeemer -> PlutusV2.ScriptContext -> Bool
mkCTValidator _ (MkMySillyRedeemer r) _ = traceIfFalse "expected 42" $ r == 42
{-# INLINABLE mkCTValidator #-}

wrappedMkVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVal = wrap mkCTValidator
{-# INLINABLE wrappedMkVal #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedMkVal ||])
```

It is possible to convert any instance of `ToData` into a `JSON` of the right format thanks to the code in the `Utilities` folder.


# ScriptContext

```haskell
:i ScriptContext
type ScriptContext :: *
data ScriptContext = ScriptContext {scriptContextTxInfo  :: TxInfo,
                                    scriptContextPurpose :: ScriptPurpose}
```

```haskell
:i ScriptPurpose
type ScriptPurpose :: *
data ScriptPurpose
  = Minting Plutus.V1.Ledger.Value.CurrencySymbol
  | Spending TxOutRef
  | Rewarding Plutus.V1.Ledger.Credential.StakingCredential
  | Certifying Plutus.V1.Ledger.DCert.DCert
```

```haskell
TxInfo	 
    txInfoInputs :: [TxInInfo]
    txInfoReferenceInputs :: [TxInInfo]
    ...
    ...
    txInfoValidRange :: POSIXTimeRange
    ...
```
`txInfoInputs` is the UTXOs we want to sped and that after the trasnaction won't exist anymore as they will be spent. Each input points to an output (not sure why).

`txInfoReferenceInputs` is still a list of UTXOs inputs but with different intent, which is that to have those inputs accesible from the transaction, as in the transaction can look at them, but it won't spend them as the first field. If they are in a script address the validator will not be run, and if they are in a normal wallet no need of keys to access them as info.

The good thing is that with `txInfoReferenceInputs` multiple transaction can access the same UTXO in the same block, bcause htey don't want to modify it.

Validation in Cardano happens in the wallet therefore off-chain so a failing transaction doesn't actually need to be submitted for the failure to be found. The same can be done for time limits, where we say a transaction is only valid between two times. That's what `txInfoValidRange` is for.
When the Tx is sent to the blockchain, before any scripts are run, some general checks are done from the node, for example that all the inputs are present, that the balance is correct, that all the fees are present and also that the time range is checked. Those are pre-validations checks, if the Tx doesn't fall in between the `txInfoValidRange` time range than validation fails immediately, without ever runnign the validator script. But, if the chekcs are passed, then validation is completely deterministic again.

By default all Txs use infinite time range. Cardano doesn't use POSIX time but uses slots time. But Plutus uses POSIX time (Real time). So we need to covnert between the two, which is an easy task if slot lenght is fixed, which is now, but it may not be in the future. That's why the upper bound, if not infinite, it has a limit that must be not to far in teh future, in case the slot lenght changes. At the moment that time limit is 36 hours. To notice that this upper bound limit will be flagged only onchain, Plutus will allow us to specify any time limit, so you can set a time limit as far as 10 years into the future with Plutus, but you'll be able to trigger that action wiht a transaction only when the time is close enough to that, probably (10 years - 36 hours).

```haskell
import Plutus.V1.Ledger.Interval
```

```haskell
type POSIXTimeRange = Interval POSIXTime

newtype POSIXTime = {getPOSIXTime :: Integer}
-- POSIX time is measured as the number of milliseconds since 1970-01-01T00:00:00Z
  

data Interval a = Interval {
                              ivFrom :: LowerBound a 
                             ,ivTo :: UpperBound a
                           }	 

data LowerBound a = LowerBound (Extended a) Closure	 

type Closure = Bool
-- Whether a bound is inclusive or not.

data Extended a = NegInf | Finite a | PosInf
-- A set extended with a positive and negative infinity.
```

```haskell
member :: Ord a => a -> Interval a -> Bool
from :: a -> Interval a
to :: a -> Interval a 
overlaps :: (Enum a, Ord a) => Interval a -> Interval a -> Bool 
contains :: Ord a => Interval a -> Interval a -> Bool 
-- and more.
```

# Parametrized Contracts

So far we achieved variability in a contract by the use of the Datum.

We can achieve variability also through the contract parameters.

That is achieved by hard coding the parameters into the contract.

The difference is that in the Datum approach you'd have only one address for every specific case, while with contract parameters you'd have a contract address for each specific case and values. Is a bit of an art to decide when to parametrise a contract, with only Datum is easier to track everything being only one contract address. In our case could be useful to use an hybrid, where only the beneficiary is parametrised while the deadline is in the datum, so that you get a different address for each beneficiary, but for each beneficiary you get many deadlines.

```haskell
data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
-- makeLift ''VestingParams
-- we don't ned this ^ any longer

{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
```

also this modifications are required:

```haskell
{-# INLINABLE  mkWrappedVestingValidator #-}
-- mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap . mkVestingValidator

validator :: Validator
-- validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator p ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- saveVal :: IO ()
saveVal :: VestingParams -> IO ()
saveVal = writeValidatorToFile "./assets/vesting.plutus" . validator
```

This will compile, but if we try to use it

```haskell
saveVal $ VestingParams "50xc..ea991" 123

-- error Reference to a name which is not an INLINABLE function: Variable p
```

This is because `p` is only known at run time, while the splicing of the Haskell template is run before compilation, so it doesn't know what `p` is.

We need to get our hands on the compiled version of `p` so that we can apply it to the function `mkWrappedVestingValidator` that has been compiled in Plutus Core. To apply an arugment to a Plutus Core function we use ``applyCode``.

To make a value that would run at run time available during compile time instead we use `liftCode` `a -> CompiledCode` (it doesn't work on functions, only data, that's why we can't use it all the time.).

so here the modification that woudl mak our code work:

```haskell
data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
makeLift ''VestingParams

-- ..

validator :: VestingParams -> Validator
validator params = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params)
```

because `liftCode` only gets data types, while our `p`/`params` is of type `VstingParams` we will still get some problems if used on its own.

That's why we need to use `makeLift`, this is not other than another Haskel Template mechanism that allows us to use Lift for our own types and lift them to the Plutus Core level.

# Reference Script

With Vasil is possile to create a trasnaction that has as an output a validator script. Others can then reference that script to interact with the validator rather than attach the script every time. This saves a lot of space on teh blockchain further than making transactions less expensive.

So you'll only have to point to the UTXO where the validator script is attached. A pointer is in general much smaller than a full script.

But where shall that validator UTXO be sent to?

If you are jsut experimenting you can simply send it to yourself so you can recollect the money spent for it. But for seriosu production application should be sent to burn script (like the one we saw previously).

In that way, when is burnt, it is guaranteed that nobody can spend it ann the validator will always be there.
