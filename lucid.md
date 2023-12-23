To convert Plutus data to the custom data type "Listing", the "Data.to" function is used. It takes two arguments: the Plutus data and the JSON schema of the custom data type. The Plutus data is an object that matches the shape of the custom data type, and the JSON schema provides the necessary information to convert the Plutus data to the custom data type.

```ts
import { Data } from "https://deno.land/x/lucid/mod.ts";

const Listing = Data.Object({
  owner: Data.Bytes(),
  amount: Data.Integer(),
  private: Data.Boolean(),
});
```

Additionally we can derive a TypeScript type definition from the data structure (creates a TypeScript type definition for the custom data type "Listing".):

```ts
type Listing = Data.Static<typeof Listing>;
```

Cast to plutus data:

```ts
const listing = Data.to<Listing>(
  { owner: "31313131313131", amount: 5252352323n, private: false },
  Listing,
);
```

To convert the custom data type back to Plutus data, the "Data.from" function is used. It takes two arguments: the serialized Plutus data and the JSON schema of the custom data type. The serialized Plutus data is a hexadecimal string that represents the Plutus data, and the JSON schema provides the necessary information to convert the custom data type to Plutus data.

Cast from plutus data:

```ts
const listing: Listing = Data.from<Listing>(
  "d8799f47313131313131311b0000000139108943d87980ff",
  Listing,
);
```
