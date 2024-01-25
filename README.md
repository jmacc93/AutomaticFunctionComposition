
The file `function_composer.js` is for automatic function composition (AFC) in javascript. AFC is composing new functions from a list of function signatures, and a given target input type set and output type

It is well commented, and is hopefully very obvious

See `findFunctions` at the bottom of the file, primarily, to see how it works

Example usage:
```js
const sigs = [
  {name: 'times2', output: 'O', inputs: ['Y'], func:(x)=>x*2},
  {name: 'add1', output: 'Y', inputs: ['X'], func:(x)=>x+1}
]
const fns = [...findFunctions(sigs, [{name:'x', type:'X'}], 'O', {})]
assertEquals(fns.length, 1)
assertEquals(fns[0](4), 10)
```
