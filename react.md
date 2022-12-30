
`useState` returns an array with a value and a function to update that state value.

```js
import React, { useState } from 'react';

function app() {
    cosnt [count, setCount] = useState(4);

    function decrementCount() { 
        setCount(prevCount => prevCount-1) 
        // use a function to make sure you don't always use the same original value from state
    }

    return (
        <>
            <button onClick={decrementCount}>-</button>
            <span>{count}</span>
            <button>+</button>
        </>
    )
}
```

