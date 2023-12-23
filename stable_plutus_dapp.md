we have two validators, one of them being an oracle, two minting policies.

There are different algo for different stable coins, we are going to implement an overcollateralised stable coin, that uses LIQUIDATION MECHANISMS to incentivise price stability.

We will use a validator to *lock* and *release* collateral ADA. And a minting policy to manage the *minting* and *burning* for stablecoins.

The value of the collateral must exceed the value of the coins minted, this extra value that is locked but can't be minted, is the reward that someone takes when they liquidate someone else's position.

Liquidating means burning the same amount of coins that someone else minted so to get their collateral. You can liquidate someone only if their positon ratio betweeen ``locked collateral / minted coins`` is below a certain predefined threshold, let's say ``150%``.

If value of ADA goes up,   you can mint more.
If value of ADA goes down, you can add more collateral or burn stable coins.

Otherwise if you go below ``150%`` someone else will liquidate your position and get that extra value.

The value of the stable is dependent on the locked collateral.

Throught the use of an oracle we'll calculate the amount of collateral needed so to peg to the dollar.

## Model

The developer need to provide two Validators

1. Validator for the oracle that supplies the USD price
2. The collateral Validator to lock the ada collateral

and the Minting policy to mint and burn stable coins.