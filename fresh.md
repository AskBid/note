```sh
$ deno run -A -r https://fresh.deno.dev project-name
```

After setting up at least the root route `routes/index.tsx`

```sh
$ deno task start 
```
# File Structure

After startin a project you can see in `deno.json` all the commands you can use from CLI.

The `start` script looks into `dev.ts` which links to `main.ts` which starts the server and links to `./fresh.gen.ts`. This last file is managed by fresh itself, we shall not touch it and contains all the routes we have, the islands and base url.

`./fresh.gen.ts` also imports all of pages. as `./islands/Counter.tsx` for instance. The rutes within `[]` are dynamic routes.

The `./routes/index.tsx` imports the island `./islands/Counter.tsx`. We also import `preact` that makes sure we can wirte `jsx` in preact islands.

We say how things trickle down from `deno.json`. Now if the server is running, whenever we add a page to `/routes` the `./fresh.gen.ts` will be updated with the added page to the list of imported pages. 

# File System Routing

If we delete all the boiler plate pages present in islands and routes, we can start a projet afresh to understand further Fresh workings.

The `./fresh.gen.ts` will be empty and we will get 404 in the browser, if the server is still running.

Let's create the base url page with a file `./routes/index.tsx`

and we can now import a react component that will now be understood from typescript.

`./routes/index.tsx`
```jsx
export default function index() {
  return (
    <div>index</div>
  )
}
```

Now if we create a new about page, we need to import the same:

> if you use JavaScript you will create `.jsx` files, if you use TypeScript, you will use `.tsx` files. The x stands for JSX. 

`./routes/about.jsx`
```jsx
export default function about() {
  return (
    <div>about</div>
  )
}
```

### Dynamic Routing

To create a dynamic route we have to call the file somethign like `[user_id].tsx` or `[user_id].jsx`.

if we add `props` with `:any` (to make TypeScript happy) and we try to `console.log` it, nothing will appear on the browser, because that stage happens on the Deno server, so we can see it on the terminal where the server is running every time we load the page. Looking then insie the props object we can extract the user_id parameter.

`./routes/[user_id].tsx`
```tsx
export default function User(props:any) {
  console.log(props.params.user)
  return (
    <div>{props.params.user}</div>
  )
}
```

Let's now create a folder `/routes/users` and then make the dynamic route as `[id]`

`./routes/users/[id].tsx`
```tsx
export default function id(props:any) {
  return (
    <div>{props.params.id}</div>
  )
}
```







# INTRO
3 ways to get components (<header>, <nav>, <section>, <article>, <aside>, <footer>)
1. Server Side Rendering
2. Progressive hydration
3. Island Architecture  

Fresh uses 3.

1. Rendering everything on the server and then push it onto the client. User has to wait for all components to be rendered and shipped.
2. Top-Down hydration, where first we get one component, then we move to the next one. User see everything first but all the component hydration is done later on. If user goes straight to component not ready yet, it is bad.
3. Dynamic regions are a combination of HTML and Scripts capable of rehydrating themselves after rendering. island arch let's each compenent hydrate themselves independently from each other asynchronously.
   
We will use **Preact** and **JSX**. Preact is a libraryr with all features from React that works with Fresh.

Fresh is made with Deno framework. Deno allows for the heavy JS that is usually shipped to the client to be completely removed, and no JS is shipped ot client by default. Deno supports TypeScript and has no configuration or build step necessary.