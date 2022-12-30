### Create Next-App

```
yarn create next-app --typescript
```
a dialogue will ask you for the project name.

```
yarn dev
```

will launch the server

### Installing Dependencies

```
yarn add @apollo/client graphql
```

[apollo docs](https://www.apollographql.com/docs/react/)

if any package tells you in the doc to use `npm` you can always just substitute for yarn.

If you download an existing repo, the dependencies will already be in the `package.json` file and you can just run `yarn` to install them.

### Add Apollo Client and Apollo Provider

Specifing where the gql queries will be addressed to. Will have to look into `pages/_app.tsx`.

```js
import '../styles/globals.css'
import type { AppProps } from 'next/app'

function MyApp({ Component, pageProps }: AppProps) {
  return <Component {...pageProps} />
}

export default MyApp
```

Substitute the `css` with yor style framework.
Add the necessary components from apollo and connect the api to the right api.

```js
import { ApolloProvider, ApolloClient, InMemoryCache } from '@apollo/client'

const client = new ApolloClient({
  uri: "https://graphql-api.testnet.dandelion.link/",
  cache: new InMemoryCache(),
});
```

and then wrap the app component with the ApolloProvider

```js
function MyApp({ Component, pageProps }: AppProps) {
  return (
    <ApolloProvider client={client}>
      <Component {...pageProps} />
    </ApolloProvider>
  )
}

export default MyApp
```

Now any file created in `pages/` will be creating a route with the name of the file.
But let's create our directory `components`.

```bash
mkdir components
cd components
touch exampleGraphQLQuery.tsx
```

```js
// components/exampleGraphQLQuery.tsx
export default function ExampleGraphQLQuery() {
    return (
        <h1>Make a Query!</h1>
    )
}
```

in `pages/index.tsx` we add

```js
import ExampleGraphQLQuery from '../components/exampleGraphQLQuery'
```

and

```js
return (
    <>
        <h1>Hello W</h1>
        <ExampleGraphQLQuery />
    </>
  )
```

### [Different Way to Render with Nextjs with Increasing Dinamicity](https://blog.logrocket.com/why-use-next-js-apollo/)

### xxx

In our `components/exampleGraphQLQuery` we start importing Apollo's hook

```js
import { useQuery } from "@apollo/client";
```

This hook take care of the querying on data by providing a loading and error variable further to the data we want to receive from the query,

```js
//...
const QUERY = gql`
    query addressesWithToken($assetName: Hex = "506f72636f526f73736f546f6b656e") {
        transactions(where: {outputs: {tokens: {asset: {assetName: {_eq: $assetName}}}}}) {
            outputs {
                address
            }
        }
    }
`;

export default function HoldersByAssetID() {

    const assetName = "506f72636f526f73736f546f6b656e"

    const { data, loading, error } = useQuery(QUERY, {
        variables: {
            assetName: assetName
        }
    });
//...
```

And now if I get `error` I can render something, if I get `loading` I can render something else, and if I get `data` I can render the data in the page.

```js
//...
    return(
        <>
            <p>{JSON.stringify(data)}</p>
            {data.uxos.map(apples: any) => <p>{apples.transaction.hash}#{apples.index} has {apples.value} lovelaces</p>}
        </>
    )
}
```

## [`Transaction` mesh](https://mesh.martify.io/apis/transaction)

[source example from gimbalabs](../ppbl-front-end-template/components/transactions/quickSendToken.tsx)

```js
import { Transaction } from '@martifylabs/mesh'
```

```js
const tx = new Transaction({ initiator: wallet })
  .sendValue(
    formik.values.address,
    {
      output: {
        amount: [
          {
            unit: "lovelace",
            quantity: "5000000"
          },
          {
            unit: assetId,
            quantity: "1",
          }
        ]
      }
    },
  );
const unsignedTx = await tx.build();
const signedTx = await wallet.signTx(unsignedTx);
const txHash = await wallet.submitTx(signedTx);
setSuccessfulTxHash(txHash)
```

## [Locking with Mesh](../ppbl-front-end-template/components/faucets/FaucetLockingComponentWithMetadata.tsx)

```js
const tx = new Transaction({
    initiator: wallet, parameters: protocolJson, era: "ALONZO"
  }).sendAssets(
    contractAddress,
    tokensToLock,
    { datum: datum }
  );
const unsignedTx = await tx.build();
const signedTx = await wallet.signTx(unsignedTx);
const txHash = await wallet.submitTx(signedTx);
setSuccessfulTxHash(txHash)
```






