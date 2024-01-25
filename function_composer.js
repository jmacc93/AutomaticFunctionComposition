/*
  This file contains, in particular, some functions to automatically compose functions given a list of function signatures, a list of input types, and an output type
  
  The culminant function is `findFunctions` at the bottom of the file
  See the comment above that function to see how to use it
  
  Most functions have comments describing them, and most have tests which demonstrate how to use them
  
  Written by Joseph Miller
*/

let print = console.log
function echo(x) { print(x); return x }

/*
 This section has a bunch of non-AFC-specific functions that we use for AFC later
*/

const tests = []
function addTest(name, func) {
  tests.push({name: name, func: func})
}
function runTests() {
  let failedCount = 0
  for(const t of tests) {
    try {
      t.func()
    } catch(err) {
      print(`Test failed: "${t.name}"\n${err.stack}`)
      failedCount++
    }
  }
  if(failedCount > 0)
    print(`${failedCount} tests failed`)
}

// turns an object into a string
function toStr(obj) {return JSON.stringify(obj)}

// true if `elem` is in `array`
function arrayContains(array, elem) {
  // first must be an array
  if(!Array.isArray(array))
    return false
  
  // one of array's elements must be `equals` given elem
  return array.some(x => equals(x, elem))
}
addTest('arrayContains', () => {
  assert(arrayContains([1, 2, 3], 1))
  assert(!arrayContains([1, 2, 3], 100))
  assert(arrayContains([[1, 2], [3, 4]], [1, 2]))
  assert(!arrayContains([[1, 2], [3, 4]], 'asdf'))
})

// true if `arrayA` and `arrayB` have the same elements
function setEquals(arrayA, arrayB) {
  // assumes arrayA and arrayB are arrays
  if(!Array.isArray(arrayA) || !Array.isArray(arrayB))
    return false
  
  if(arrayA === arrayB) // they're the same object in memory
    return true
  // must have same length
  if((arrayA.length !== arrayB.length))
    return false
  
  // every elem in A must be in B
  return arrayA.every(elem => arrayContains(arrayB, elem))
}

function equals(objA, objB) {
  if(objA === objB)
    return true
  
  const typeA = typeof objA
  if(typeA !== typeof objB)
    return false
  
  if(typeA === 'object') {
    // array
    if(Array.isArray(objA)) {
      if(!Array.isArray(objB)  ||  (objA.length !== objB.length))
        return false
      for(let i = 0; i < objA.length; i++) {
        if(!equals(objA[i], objB[i]))
          return false
      }
      return true
    } else { // a generic object
      for(const key in objA) {
        if(!(key in objB)  ||  !equals(objA[key], objB[key]))
          return false
      }
      for(const key in objB) {
        if(!(key in objA)  ||  !equals(objA[key], objB[key]))
          return false
      }
      return true
    }
  }
  return false
}
addTest('equals', () => {
  assert(equals([1, 2, 3], [1, 2, 3]))
  assert(!equals([1, 2, 3], [1, 2, 100]))
  assert(!equals([1, 2, 3], [1, 100, 3]))
  assert(!equals([1, 2, 3], [3, 2, 1]))
  assert(!equals([], [100]))
  assert(equals(123, 123))
  assert(!equals(123, 999))
  assert(equals('asdf', 'asdf'))
  assert(!equals('asdf', 'xxxx'))
  assert(equals({a: 1}, {a: 1}))
  assert(!equals({a: 1}, {a: 1, b: 2}))
})

function assert(x, msg) {
  if(!x)
    throw Error(msg ?? `Assertion error`)
}
function assertEquals(given, expected, msg) {
  if(!equals(given, expected)) {
    if(msg == undefined)
      throw Error(`assertEquals failed\ngiven = ${toStr(given)}\nexpected = ${toStr(expected)}`)
    else
      throw Error(msg + `\na = ${toStr(given)}\nexpected = ${toStr(expected)}`)
  }
}
function assertEqualsUnordered(givenArray, expectedArray, msg) {
  if(!setEquals(givenArray, expectedArray)) {
    if(msg == undefined)
      throw Error(`assertEqualsUnordered failed\ngiven = ${toStr(givenArray)}\nexpected = ${toStr(expectedArray)}`)
    else
      throw Error(msg + `\ngiven = ${toStr(givenArray)}\nexpected = ${toStr(expectedArray)}`)
  }
}

function last(lst) { return lst[lst.length - 1] }

// Removes everything in `lstB` from `lstA`, compares using the `equals` function above
function removeAll(lstA, lstB) {
  for(const valB of lstB) {
    let i = 0
    while(i < lstA.length) {
      if(equals(lstA[i], valB))
        lstA.splice(i, 1) // remove
      else
        i++
    }
  }
  return lstA
}
addTest('removeAll', () => {
  assertEquals(removeAll([1, 2, 3], [2, 3]), [1])
  assertEquals(removeAll([1, 2, 3], [1, 2, 3]), [])
  const arr1 = [1, 2, 3]
  const arr2 = [2]
  removeAll(arr1, arr2)
  assertEquals(arr1, [1, 3])
})

// Applies `.flat()` repeatedly to `lst` until its result no longer changes
function flattenCompletely(lst) {
  assert(Array.isArray(lst))
  let ret = lst
  let step = 10
  while(true) {
    const flatRet = ret.flat()
    if(equals(flatRet, ret))
      return flatRet
    ret = flatRet
  }
}
addTest('flattenCompletely', () => {
  assertEquals(flattenCompletely([[1, 2], [3, 4]]), [1, 2, 3, 4])
  assertEquals(flattenCompletely([]), [])
  assertEquals(flattenCompletely([1, 2, 3]), [1, 2, 3])
})

// Flatten and remove duplicates from `lst`
function flatDedupe(lst) {
  return [... new Set(flattenCompletely(lst))].sort()
}
addTest('flatDedupe', () => {
  assertEquals(flatDedupe([1, [1, [2, 3, 2], 3, 4]]), [1, 2, 3, 4])
  assertEquals(flatDedupe([]), [])
  assertEquals(flatDedupe([[[1, 2, 3]]]), [1, 2, 3])
})


function truePred(x) { return true }

/*
  listOfInts is like [i1, i2, i3, ...] where the `in` are integers
  yield combinations like [j1, j2, ...] where `0 <= jn <= in`
*/
function* generateIntegerCombinations(listOfInts) {
  const len = listOfInts.length
  const ret = Array(len)
  for(let i = 0; i < len; i++)
    ret[i] = 0
  while(true) {
    // think spedometer; we flip over one card then if its over the limit, we flip over the next
    yield ret.slice()
    for(let i = 0; i < len; i++) {
      ret[i]++
      if(ret[i] > listOfInts[i]) {
        if(i == len - 1) // reached the last card to flip
          return
        ret[i] = 0
      } else {
        break
      }
    }
  }
}
addTest('generateIntegerCombinations', () => {
  const res = [...generateIntegerCombinations([1, 2, 3])]
  assertEquals(res, [[ 0, 0, 0 ], [ 1, 0, 0 ], [ 0, 1, 0 ], [ 1, 1, 0 ], [ 0, 2, 0 ], [ 1, 2, 0 ], [ 0, 0, 1 ], [ 1, 0, 1 ], [ 0, 1, 1 ], [ 1, 1, 1 ], [ 0, 2, 1 ], [ 1, 2, 1 ], [ 0, 0, 2 ], [ 1, 0, 2 ], [ 0, 1, 2 ], [ 1, 1, 2 ], [ 0, 2, 2 ], [ 1, 2, 2 ], [ 0, 0, 3 ], [ 1, 0, 3 ], [ 0, 1, 3 ], [ 1, 1, 3 ], [ 0, 2, 3 ], [ 1, 2, 3 ]])
})

// yields combination lists each with one element from each sublist in given list
function* generateAllCombinations(lst) {
  assert(lst.every(sub => sub.length > 0), `Cannot generate elements from an empty list! Given ${JSON.stringify(lst)}`)
  for(const combo of generateIntegerCombinations(lst.map(sublst => sublst.length - 1)))
    yield combo.map((i, j) => lst[j][i])
}
addTest('generateAllCombinations', () => {
  const res = [...generateAllCombinations([[1, 2], [2, 3]])]
  assertEquals(res, [ [ 1, 2 ], [ 2, 2 ], [ 1, 3 ], [ 2, 3 ] ])
})


// yields [key, value] pairs from `obj`
function* keyValuesOf(obj) {
  for(const key in obj)
    yield [key, obj[key]]
}

/*
  Recursively traverses a nested object or array structure,
  yields pairs of [path, part] for each part object encountered during traversal
  Where path represents the path (list of indices or keys) to reach the yielded part within the given `obj`
*/
function* eachPathAndObject(obj) {
  yield [[], obj]
  const iteratorStack = [keyValuesOf(obj)]
  const pathStack = []

  while(iteratorStack.length > 0) {
    const iterator = last(iteratorStack)
  
    const {value, done} = iterator.next()
    if(done) {
      iteratorStack.pop()
      pathStack.pop()
      continue
    }
    const [key, childNode] = value

    pathStack.push(key)
    yield [pathStack, childNode]
    
    if(typeof childNode === 'object'  &&  childNode != null)
      // only push if childNode is indexable
      iteratorStack.push(keyValuesOf(childNode))
    else 
      pathStack.pop()
  }
}
addTest('eachPathAndObject', () => {
  const obj0 = 10 
  const obj10 = 11
  const obj11 = 12
  const obj1 = [obj10, obj11]
  const obj2a = 13
  const obj2b0 = 14
  const obj2b10 = 15
  const obj2b1 = [obj2b10]
  const obj2b = [obj2b0, obj2b1]
  const obj2 = {a: obj2a, b: obj2b}
  const obj = [obj0, obj1, obj2]
  const paths = [
    [[], obj],
    [['0'], obj0],
    [['1'], obj1],
    [['1', '0'], obj10],
    [['1', '1'], obj11],
    [['2'], obj2],
    [['2', 'a'], obj2a],
    [['2', 'b'], obj2b],
    [['2', 'b', '0'], obj2b0],
    [['2', 'b', '1'], obj2b1],
    [['2', 'b', '1', '0'], obj2b10],
  ]
  const res = []
  for(const [path, node] of eachPathAndObject(obj))
    res.push([path.slice(), node])
  assertEquals(res, paths)
})

// returns the part of `obj` referred to by the given `path`
function getAtPath(path, obj) {
  let node = obj
  for(const key of path)
    node = node[key]
  return node
}
addTest('getAtPath', () => {
  const obj0 = 10 
  const obj10 = 11
  const obj11 = 12
  const obj1 = [obj10, obj11]
  const obj2a = 13
  const obj2b0 = 14
  const obj2b10 = 15
  const obj2b1 = [obj2b10]
  const obj2b = [obj2b0, obj2b1]
  const obj2 = {a: obj2a, b: obj2b}
  const obj = [obj0, obj1, obj2]
  for(const [path, node] of eachPathAndObject(obj))
    assert(getAtPath(path, obj))
})
addTest('getAtPath 2', () => {
  assertEquals( getAtPath(['b'],         {a: 1, b: 2}), 2)
  assertEquals( getAtPath(['b', 0],      {a: 1, b: [2, 3]}), 2)
  assertEquals( getAtPath(['b', 1],      {a: 1, b: [2, 3]}), 3)
  assertEquals( getAtPath(['b', 1, 'c'], {a: 1, b: [2, {c: 3, d:4}]}), 3)
})

// Sets `obj[key][subkey] = value`, and creates `obj[key]` if it doesn't exist
function addToProp(obj, key, subkey, value) {
  assert(typeof obj === 'object')
  let sub = obj[key]
  if(sub == undefined) {
    sub = {[subkey]: value}
    obj[key] = sub
  } else {
    assert(typeof sub === 'object')
    sub[subkey] = value
  }
}


// -----------------------------------
// -----------------------------------

/*
  This section is for subset relations
  Sets here are either compound or atomic
    Atomic sets are strings
    Compound sets are objects representing either set intersections, or unions
*/

// Note: all the subset relations here are improper (ie: can also be equal)

/*
  Subset databases are like:
    {'SomeSet': {'SomeSubset': true, ...}, ...}
    But don't construct these manually, use addSubsetRelation(superset, subset, setsDB)
    to add sets to `setsDB`

  Atomic sets (sets that aren't compositions of other sets in set expressions) are strings
  Set intersections are like:
    {kind: 'intersection', terms: [ ... ]}
  Set unions are like:
    {kind: 'union', terms: [ ... ]}
*/

function isAtomicSet(obj) { return  typeof obj === 'string'  &&  obj.length > 0 }
function isSetIntersection(obj) { return typeof obj === 'object'  &&  obj.kind === 'intersection' }
function isSetUnion(obj) { return typeof obj === 'object'  &&  obj.kind === 'union' }
function isValidSetExpression(obj) {
  const type = typeof obj 
  if(type === 'string')
    return true
  if(type !== 'object' ||  Array.isArray(obj))
    return false
  const kind = obj.kind
  if(kind == undefined)
    return false
  if(kind !== 'intersection'  ||  kind != 'union')
    return false
  return obj.terms.every(isValidSetExpression)
}
function setIntersection(...terms) {
  assert(terms.length > 0)
  if(terms.length === 1)
    return terms[0]
  assert(terms.every(isValidSetExpression))
  return {kind:'intersection', terms: terms}
}
function setUnion(...terms) {
  assert(terms.length > 0)
  if(terms.length === 1)
    return terms[0]
  assert(terms.every(isValidSetExpression))
  return {kind:'union', terms: terms}
}

/*
  allSupersets and allSubsets return the supersets/subsets of atomic set aset registered in setsDB
  Important note:
    For a setsDB object to be of proper form:
    If A < B < C, then both A < B and A < C are in setsDB
    The function addSubsetRelation preserves this property
*/
function* allSupersets(aset, setsDB) {
  assert(isAtomicSet(aset))
  // scan through all sets and yield only the ones whose setsDB entry contains aset
  for(const asuperset in setsDB) {
    if(aset in setsDB[asuperset])
      yield asuperset
  }
}
addTest('allSupersets', () => {
  const db = {
    'A': {'X': true, 'Y': true},
    'B': {'A': true, 'X': true, 'Y': true},
    'C': {'X': true}
  }
  assertEqualsUnordered([...allSupersets('X', db)], ['A', 'B', 'C'])
})
function* allSubsets(aset, setsDB) {
  // get aset's setsDB entry and yield everything in it
  const subsetObj = setsDB[aset]
  if(subsetObj == undefined)
    return
  for(const subset in subsetObj) {
    assert(subsetObj[subset] === true)
    yield subset
  }
}
addTest('allSubsets', () => {
  const db = {
    'A': {'X': true, 'Y': true},
    'B': {'A': true, 'X': true, 'Y': true},
    'C': {'X': true}
  }
  assertEqualsUnordered([...allSubsets('X', db)], [])
  assertEqualsUnordered([...allSubsets('B', db)], ['A', 'X', 'Y'])
  assertEqualsUnordered([...allSubsets('C', db)], ['X'])
})

/*
  setsDB should have the property that if A < B < C, then A < B and A < C should be in setsDB
  (where A < B means A is a subset of B)
  ie: if A is a subset of B which is a subset of C then `('A' in setsDB['B']) && ('A' in setsDB['C'])`
  So every entry setsDB['X'] contains *all* subsets
  If we just used `setsDB['Y']['Z'] = true` to define subset relations, then this property wouldn't be preserved since if `setsDB['X']['Y']` then after that assignment we wouldn't have `setsDB['X']['Z']`
  Instead we use `addSubsetRelation` to preserve this property
*/
function addSubsetRelation(aset, newSubset, setsDB) {
  // TODO: add condition for when aset is a union (note: intersection case is probably not useful)
  // note: this function doesn't check for cyclic relationships
  assert(isAtomicSet(aset))
  assert(isAtomicSet(newSubset))
  // add newSubset and all of its subsets to aset and its supersets
  const subsetsToAdd = [newSubset, ...allSubsets(newSubset, setsDB)]
  for(const subset of subsetsToAdd)
    addToProp(setsDB, aset, subset, true)
  for(const superset of allSupersets(aset, setsDB)) {
    for(const subset of subsetsToAdd)
      addToProp(setsDB, superset, subset, true)
  }
}
addTest('addSubsetRelation', () => {
  const db = {}
  addSubsetRelation('b', 'a', db)
  addSubsetRelation('c', 'b', db)
  addSubsetRelation('d', 'c', db)
  // d -> c -> b -> a
  assertEquals(db, {b: {a: true}, c: {b: true, a: true}, d: {c: true, a: true, b: true}})
  
  addSubsetRelation('b', 'x', db)
  // d -> c -> b -> a
  //             \> x
  assertEquals(db, {b: {a: true, x: true}, c: {b: true, a: true, x: true}, d: {c: true, a: true, b: true, x: true}})
})
addTest('addSubsetRelation 2', () => {
  const db = {}
  addSubsetRelation('b', 'x', db)
  addSubsetRelation('d', 'c', db)
  addSubsetRelation('c', 'b', db)
  addSubsetRelation('b', 'a', db)
  assertEquals(db, {b: {a: true, x: true}, c: {b: true, a: true, x: true}, d: {c: true, a: true, b: true, x: true}})
})

/*
  An atomic set is a non-empty string
  This checks setsDB[atomicSetA][atomicSetB] is true
*/
function isSubsetOfAtomic(atomicSetA, atomicSetB, setsDB) {
  // Note: this is improper subset, so A = B also means A subof B
  assert(isAtomicSet(atomicSetA))
  assert(isAtomicSet(atomicSetB))
  if(atomicSetA === atomicSetB)
    return true // improper subset
  const subsets = setsDB[atomicSetB]
  if(subsets == undefined)
    return false
  return subsets[atomicSetA] ?? false
}
addTest('isSubsetOfAtomic', () => {
  const db = {
    'Y': {'X': true}
  }
  assert(isSubsetOfAtomic('X', 'X', db))
  assert(isSubsetOfAtomic('X', 'Y', db))
  assert(!isSubsetOfAtomic('X', 'Z', db))
})

/*
  The following 4 functions are for different cases for isSubset involving non-atomic / compound object sets
  The `lhs < rhsUnion` case in isSubsetRhsUnion and the `lhsIntersection < rhs` case in isSubsetLhsIntersection are approximations, this means they don't apply universally!
  The reason for this is that there is no way to reduce these two situations to boolean / logical expressions involving only atomic sets in all cases
  The assumptions required for the approximations to hold are the approximations themselves
*/
function isSubsetRhsIntersection(lhs, rhsIntersection, setsDB) {
  assert(isSetIntersection(rhsIntersection))
  return rhsIntersection.terms.every(term => isSubset(lhs, term, setsDB))
}
function isSubsetRhsUnion(lhs, rhsUnion, setsDB) { // approximation
  assert(isAtomicSet(lhs))
  assert(isSetUnion(rhsUnion))
  return rhsUnion.terms.some(term => isSubset(lhs, term, setsDB))
}
function isSubsetLhsUnion(lhsUnion, rhs, setsDB) {
  assert(isSetUnion(lhsUnion))
  return lhsUnion.terms.every(term => isSubset(term, rhs, setsDB))
}
function isSubsetLhsIntersection(lhsIntersection, rhs, setsDB) { // approximation
  assert(isSetIntersection(lhsIntersection))
  assert(isAtomicSet(rhs))
  return lhsIntersection.terms.some(term => isSubset(term, rhs, setsDB))
}

/*
  This is the subset test that you want to use
  Both `lhs` and `rhs` can be atomic (strings) or intersection / union objects
*/
function isSubset(lhs, rhs, setsDB) {
  if(isSetIntersection(rhs)) // eg: `X * Y subof A * B`
    return isSubsetRhsIntersection(lhs, rhs, setsDB)
  else if(isSetUnion(lhs)) // eg: `X + Y subof A + B`
    return isSubsetLhsUnion(lhs, rhs, setsDB)
  else if(isSetUnion(rhs)) // eg: `X * Y subof A + B`
    return isSubsetRhsUnion(lhs, rhs, setsDB) // this is an approximation
  else if(isSetIntersection(lhs)) // eg: `X * Y subof A`
    return isSubsetLhsIntersection(lhs, rhs, setsDB) // this is an approximation
  // otherwise, must be a pair of atomic sets  // eg: `X subof A`
  assert(isAtomicSet(lhs))
  assert(isAtomicSet(rhs))
  return isSubsetOfAtomic(lhs, rhs, setsDB)
}
addTest('isSubset', () => {
  const db = {}
  addSubsetRelation('Y', 'X', db)
  addSubsetRelation('Q', 'X', db)
  addSubsetRelation('Q', 'Z', db)
  addSubsetRelation('X', 'A', db)
  addSubsetRelation('P', 'X', db)
  addSubsetRelation('L', 'S', db)
  /*
   superset supset1 subset1subset1
                    subset1subset2
            subset2 subset2subset1
   
   Y X A
   
   Q X A
     Z
   
   P X
   
   L S
  */
  
  assert(isSubset('X', 'X', db))
  assert(isSubset('X', 'Y', db))
  assert(!isSubset('X', 'Z', db))
  
  assert(isSubset(setIntersection('X', 'Z'), 'X', db))
  assert(isSubset(setIntersection('X', 'Z'), 'Y', db))
  assert(isSubset(setIntersection('X', 'Z'), 'Z', db))
  assert(!isSubset(setIntersection('X', 'Z'), 'W', db))
  
  assert(isSubset(setUnion('X', 'Z'), 'Q', db))
  assert(!isSubset(setUnion('X', 'Z'), 'Y', db))
  assert(!isSubset(setUnion('X', 'Z'), 'W', db))
  
  assert(isSubset('X', setUnion('Y', 'L'), db))
  assert(!isSubset('X', setUnion('W', 'L'), db))
  assert(!isSubset('W', setUnion('Y', 'L'), db))
  
  assert(isSubset('X', setIntersection('Y', 'P'), db))
  assert(!isSubset('X', setIntersection('Y', 'W'), db))
})
/*
  areSetsEqual comes from set theory
  Two sets are equal if they both are improper subsets of eachother
*/
function areSetsEqual(lhs, rhs, setsDB) {
  return isSubset(lhs, rhs, setsDB) && isSubset(rhs, lhs, setsDB)
}
addTest('areSetsEqual', () => {
  assert(areSetsEqual('X', 'X', {}))
  assert(areSetsEqual('X', 'X', {'X': {'Y': true}}))
  
  const inter = setIntersection('X', 'Y')
  assert(areSetsEqual(inter, inter, {}))
  const inter2 = setIntersection('X', 'Z')
  assert(!areSetsEqual(inter, inter2, {}))
  
  const union = setIntersection('X', 'Y')
  assert(areSetsEqual(union, union, {}))
  const union2 = setIntersection('X', 'Z')
  assert(!areSetsEqual(union, union2, {}))
})

// -----------------------------------

// true if something in `lst` is a subset of `setObj`
function isOneASubsetOf(setObj, lst, setsDB) {
  for(const x of lst) {
    if(isSubset(x, setObj, setsDB))
      return true
  }
  return false
}

// true if something in `lstB` is a subset of something in `lstA`
function isOneASubsetOfOne(lstA, lstB, setsDB) {
  if(lstA.length === 0 || lstB.length === 0)
    return true
  return lstA.some(s => isOneASubsetOf(s, lstB, setsDB))
}
addTest('isOneASubsetOfOne', () => {
  const emptySetsDB = {}
  // true because 'c' is in both lists:
  assert(isOneASubsetOfOne(['a', 'b', 'c'], ['c', 'd', 'e'], emptySetsDB))
  // not true because 'XXX' isn't in first list:
  assert(!isOneASubsetOfOne(['a', 'b', 'c'], ['XXX'], emptySetsDB))
  // both defined to be true:
  assert(isOneASubsetOfOne(['a', 'b', 'c'], [], emptySetsDB))
  assert(isOneASubsetOfOne([], ['a', 'b', 'c'], emptySetsDB))
})

// true if everything in `lstA` is a superset of something in `lstB`
function areAllSetsInSetList(lstA, lstB, setsDB) {
  return lstA.every(s => isOneASubsetOf(s, lstB, setsDB))
}
addTest('areAllSetsInSetList', () => {
  const emptySetsDB = {}
  // true because 'a' and 'b' are in both lists
  assert(areAllSetsInSetList(['a', 'b'], ['a', 'b', 'c'], emptySetsDB))
  // not true because 'a', 'b', 'c' don't appear in second list
  assert(!areAllSetsInSetList(['a', 'b', 'c'], ['XXX'], emptySetsDB))
  // defined true:
  assert(areAllSetsInSetList([], ['a', 'b', 'c'], emptySetsDB))
  assert(areAllSetsInSetList([], ['XXX'], emptySetsDB))
  assert(areAllSetsInSetList([], [], emptySetsDB))
})

// -----------------------------------

/*
  Imagine a network of nodes, where every node and edge has a light that can be on or off
  The following section is for lighting up nodes and edges adjacent to already lit-up nodes and edges
  Specifically:
  The nodes are types, and the edges between nodes are function signatures (sigs)
  
  For the different directions (down / dn, up):
  Here, if X node or signature contains Y thing in its definition, then Y is down stream from X
  If Y is downstream from X, then X is upstream from Y
*/

/*
  A note about `allSigs`:
  Signature objects should look like:
  {name: 'someName', output: TYPE, inputs: [TYPE, TYPE, ...], func: (...) => {...}}
  Where TYPE are either atomic set strings, or set intersection / union objects
  The `func` property is only necessary when turning app objects into real functions
  ie all sig lists after `findApps` should have `func` properties
*/

/*
  `sigsDn` takes a list of types and returns a list of sigs that produce at least one of those types
  This is for finding edges downstream from nodes
*/
function sigsDn(allSigs, types, setsDB) {
  return allSigs.filter(sig => isOneASubsetOf(sig.output, types, setsDB))
}
addTest('sigsDn', () => {
  const sigs = [
    { name: 'Fo', output: 'O', inputs: [ 'A1' ] },
    { name: 'f1', output: 'A1', inputs: [ 'A2' ] },
    { name: 'f2', output: 'A2', inputs: [ 'A3' ] },
    { name: 'Fx', output: 'A3', inputs: [ 'X' ] }
  ]
  const dnSet = sigsDn(sigs, ['A1'], {})
  // find all sigs from list above whose output is 'A1'
  assertEquals(dnSet, [{ name: 'f1', output: 'A1', inputs: [ 'A2' ] }])
})

/*
  `satisfiedSigsUp` finds a list of sigs whose inputs contain all of the given `nodes`
  A sig is "satisfied" if it could be applied to a list of arguments with the given types
*/
function satisfiedSigsUp(allSigs, types, setsDB) {
  return allSigs.filter(sig => areAllSetsInSetList(sig.inputs, types, setsDB))
}
addTest('satisfiedSigsUp', () => {
  const testAllSigs = [
    {name: 'f', output: 'A', inputs: ['X']},
    {name: 'f', output: 'B', inputs: ['A']},
    {name: 'f', output: 'C', inputs: ['A', 'Z']},
  ]
  const upsigs = satisfiedSigsUp(testAllSigs, ['A', 'Y'], {})
  // find the sigs above whose inputs are entirely in ['A', 'Y']
  assertEqualsUnordered(upsigs, [{name: 'f', output: 'B', inputs: ['A']}])
  
})

/*
  Consider the upstream, downstream terminology
  The watershed of a river (which also has an upstream and downstream) is defined as (wiktionary.org): "A region of land within which water flows down into a specified body, such as a river, lake, sea, or ocean; a drainage basin" this is analogous to a dnshed in the context of this code
  An upshed here is everything you end up at when you keep going upstream
  A satisfied upshed is everything you end up at when you keep applying `satisfiedSigsUp`
  But, this is only the satisfied upshed for sigs (edges), it doesn't include types / sets (nodes)
*/
function satisfiedSigsUpShed(allSigs, startTypes, setsDB) {
  let curSigs = []
  let curTypes = structuredClone(startTypes)
  while(true) {
    const newSigs = satisfiedSigsUp(allSigs, curTypes, setsDB)
    const nextCurSigs = flatDedupe(curSigs.concat(newSigs))
    curTypes = flatDedupe(curTypes.concat(newSigs.map(sig => sig.output)))
    // if curSigs hasn't changed then we're done, otherwise keep going
    if(equals(nextCurSigs, curSigs))
      return curSigs
    else
      curSigs = nextCurSigs
  }
}
addTest('satisfiedSigsUpShed', () => {
  const testAllSigs = [
    {name: 'e', output: 'X', inputs: ['Q']},
    {name: 'f', output: 'A', inputs: ['X']},
    {name: 'g', output: 'B', inputs: ['A']},
    {name: 'h', output: 'C', inputs: ['B']},
  ]
  const shed = satisfiedSigsUpShed(testAllSigs, ['X'], {})
  // The sat-upshed of 'X' here is everything but the e:Q->X sig because thats dnstream
  assertEqualsUnordered(shed, [
    {name: 'f', output: 'A', inputs: ['X']},
    {name: 'g', output: 'B', inputs: ['A']},
    {name: 'h', output: 'C', inputs: ['B']},
  ])
})

// -----------------------------------

/*
  The rest of this file is for finding ways to compose signatures to make new signatures
  ie: Automatic Function Composition (AFC)
  
  Thats primarily through:
    function* findComposableSigHypertree(allSigs, targetInputs, targetOutput, setsDB)
    function makeCompoundSigFromSigHypertree(treeSigs, output, setsDB)
    function* makeAppsFromCompoundSig(compoundSig, realInputs, setsDB)
    function* findApps(allSigs, realInputs, targetOutput, setsDB)
    function* findFunctions(allSigs, functionMap, realInputs, targetOutput, setsDB, appFilter = truePred)
*/

/*
  Note: a hypertree here is a directed hypergraph (a network where each edge can point to multiple nodes) that is acyclic. I don't actually check here whether the resulting hypertrees are acyclic, so...
  `findComposableSigHypertree` yields lists of sigs from `allSigs` which can be composed to make `targetOutput` from `targetInputs`
  Its algorithm is a continuation loop (see comments in the function) controlled by a backtracking algorithm
*/
const con_advanceFrontier = 0
const con_makeChoice = 1
const con_backtrack = 2
function* findComposableSigHypertree(allSigs, targetInputs, targetOutput, setsDB) {
  /* Whenever we make a choice (a decision that biases the yielded solution to a particular solution)
     we push the other possible choices to the choicepointStack
     This makes this solver into a depth-first search */
  const choicepointStack = [] // elems like current state: [solution, usedNodes, frontierNodes, choices]
  
  // current state:
  // These are all recorded in the choicepointStack whenever a choice is made
  let solution = [] // a list of the sigs whose composition makes the output given the inputs
  let usedNodes = [...targetInputs] // previously frontier nodes that cannot be frontier nodes again
  let frontierNodes = [targetOutput] // nodes whose down-edges will be used to make the solution; intermediates
  let choices = null // down-edges / sigs outputting the last selected frontier node
  
  // We start with the advanceFrontier continuation because choices == null currently
  let conI = con_advanceFrontier
  
  /* This while loop plus switch here simulates a continuation-based program
     Think: the switch's cases act like line labels, and we are repeatedly gotoing different lines
     This is identical to a stackless program jumping to different procedures
     If we used functions, each call would increase the stack depth, and so there could be stack overflows
     We could use functions and manage execution flow with return values, but its much more awkward to do 
     it like that than this elegant design
   */
  const inputSatisfiedUpshed = satisfiedSigsUpShed(allSigs, targetInputs, setsDB)
  conLoop: 
  while(true) {
    switch(conI) {
      
      // select to a new frontier node and make initial choices from that node
      // note: this falls through to makeChoice
      case con_advanceFrontier:
        
        // select a frontier node
        const selectedNode = frontierNodes.pop()
        usedNodes.push(selectedNode)
        
        // construct new choices list
        choices = sigsDn(inputSatisfiedUpshed, [selectedNode], setsDB)
        choicepointStack.push([solution.slice(), usedNodes.slice(), frontierNodes.slice(), choices])
        
        // explicit fallthrough to makeChoice
        
      // choose from current choices, add the chosen edge's inputs as frontier nodes, make a choicepoint
      case con_makeChoice:

        // must have choices
        if(choices.length === 0) {
          // no more choices here
          conI = con_backtrack; continue conLoop // backtrack
        }
        // else, do have choices
        
        // make a choice
        const chosenSig = choices.splice(0, 1)[0] // extract first from choices; splice(0,1) removes first & rets it in lst
        solution.push(chosenSig)
        const newFrontierNodes = chosenSig.inputs
        
        // construct new frontier nodes list without inputs or used nodes
        frontierNodes = flatDedupe(frontierNodes.concat(newFrontierNodes))
        removeAll(frontierNodes, targetInputs)
        removeAll(frontierNodes, usedNodes)
        
        // do we still have any frontier nodes?
        if(frontierNodes.length === 0) {
          // must have solved system successfully because sln in supshed of target inputs and dnshed of target output
          yield solution
          conI = con_backtrack; continue conLoop // backtrack
        }
        // else, we have frontier nodes
        
        // advance to new frontier node
        conI = con_advanceFrontier; continue conLoop
        
      // revert state to old choicepoint state
      // note: The same `choices` data that is popped when a choice is made is stored in 
      //       the choicepoint stack, so when we pop a choicepoint the choices variable doesn't
      //       include the previous choice. This is how we exhaust all the choices at the choicepoint
      case con_backtrack:
        
        // must have choicespoints
        if(choicepointStack.length === 0) {
          // done
          return
        }
        // else, have choicepoints:
        
        // restore state to old choicepoint state
        const oldState = last(choicepointStack)
        solution = oldState[0].slice(); usedNodes = oldState[1].slice()
        frontierNodes = oldState[2].slice(); choices = oldState[3]
        
        // remove choicepoint if it has no choices left
        if(choices.length <= 1)
          choicepointStack.pop()
        
        // continue at make choice section
        conI = con_makeChoice; continue conLoop // don't fallthrough
        
    }
  }
}

const findComposableSigHypertreeTestsDat = [
  {
    name: 'sigs1',
    sigs: [
      {name: 'f', output: 'A', inputs: ['X']},
      {name: 'g', output: 'O', inputs: ['A', 'Y']},
      {name: 'ha', output: 'B', inputs: ['A']},
      {name: 'ho', output: 'C1', inputs: ['O']},
      {name: 'hc', output: 'O', inputs: ['C2']},
      {name: 'hy', output: 'C3', inputs: ['Y']},
      {name: 'hc2', output: 'Y', inputs: ['C4']}
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "g", output: "O", inputs: ["A", "Y"]},
        {name: "f", output: "A", inputs: ["X"]}
      ]
    ]
  },
  {
    name: 'sigs2',
    sigs: [
      {name: 'f', output: 'C', inputs: ['O', 'B']},
      {name: 'g', output: 'O', inputs: ['X', 'Y']},
      {name: 'h', output: 'A', inputs: ['Z']},
      {name: 'F', output: 'O', inputs: ['A']},
      {name: 'G', output: 'D', inputs: ['A']},
      {name: 'H', output: 'A', inputs: ['E']}
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "g", output: "O", inputs: ["X", "Y"]}
      ]
    ]
  },
  {
    name: 'multiple inputs, multiple solutions',
    sigs: [
      {name: 'f', output: 'A', inputs: ['X']},
      {name: 'g', output: 'O', inputs: ['A', 'Y']},
      {name: 'h', output: 'B', inputs: ['Y']},
      {name: 'j', output: 'O', inputs: ['X', 'B']},
      {name: 'F', output: 'C', inputs: ['O']},
      {name: 'G', output: 'D', inputs: ['A']},
      {name: 'H', output: 'E', inputs: ['Y']}
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "g", output: "O", inputs: ["A", "Y"]},
        {name: "f", output: "A", inputs: ["X"]}
      ], 
      [
        {name: "j", output: "O", inputs: ["X", "B"]}, 
        {name: "h", output: "B", inputs: ["Y"]}
      ]
    ]
  },
  {
    name: 'sigs4',
    sigs: [
      {name: 'g', output: 'O', inputs: ['X', 'Y', 'Z']},
      {name: 'f', output: 'O', inputs: ['A', 'B']},
      {name: 'ha', output: 'A', inputs: ['X']},
      {name: 'hb', output: 'B', inputs: ['Y']}
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "f", output: "O", inputs: ["A", "B"]},
        {name: "hb", output: "B", inputs: ["Y"]},
        {name: "ha", output: "A", inputs: ["X"]}
      ]
    ]
  },
  {
    name: 'sigs5',
    sigs: [
      {name: 'f', output: 'O', inputs: ['X', 'A']},
      {name: 'g', output: 'A', inputs: ['B']},
      {name: 'h', output: 'B', inputs: ['Y']},
      {name: 'j', output: 'X', inputs: ['Y']},
      {name: 'fux', output: 'UX', inputs: ['X']},
      {name: 'fub', output: 'UB', inputs: ['B']},
      {name: 'fua', output: 'UA', inputs: ['A']},
      {name: 'fuy', output: 'UY', inputs: ['Y']},
      {name: 'fuo', output: 'UO', inputs: ['O']},
      {name: 'fdx', output: 'X', inputs: ['DX']},
      {name: 'fdb', output: 'B', inputs: ['DB']},
      {name: 'fda', output: 'A', inputs: ['DA']},
      {name: 'fdy', output: 'Y', inputs: ['DY']},
      {name: 'fdo', output: 'O', inputs: ['DO']}
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "f", output: "O", inputs: ["X", "A"]}, 
        {name: "g", output: "A", inputs: ["B"]}, 
        {name: "h", output: "B", inputs: ["Y"]}
      ]
    ]
  },
  {
    name: 'chain length 3',
    sigs: [
      {name: 'Fo', output: 'O', inputs: ['A1']},
      {name: 'f1', output: 'A1', inputs: ['A2']},
      {name: 'f2', output: 'A2', inputs: ['A3']},
      {name: 'Fx', output: 'A3', inputs: ['X']}
    ],
    inputs: ['X'],
    output: 'O',
    targetSolution: [
      [
        {name: "Fo", output: "O", inputs: ["A1"]}, 
        {name: "f1", output: "A1", inputs: ["A2"]}, 
        {name: "f2", output: "A2", inputs: ["A3"]}, 
        {name: "Fx", output: "A3", inputs: ["X"]}
      ]
    ]
  },
  {
    /*
      This problem's hypertree looks like
        o
      /  \   Fo1  Fo2
     | X |   f1_1_1  f1_1_2  f1_2_1  f1_2_2
     | X |   f2_1_1  f2_1_2  f2_2_1  f2_2_2
     \  /    Fx1  Fx2
      x
     Where the `fN_M_K` edges stand for unary sigs between non-terminal-connected layers
     The `FoN` edges connect to the output
     The `FxN` edges connect to the input
    */
    name: 'ladder width 2 length 3',
    sigs: [
      {name: 'Fo1', output: 'O', inputs: ['A1_1']},
      {name: 'Fo2', output: 'O', inputs: ['A1_2']},
      {name: 'f1_1_1', output: 'A1_1', inputs: ['A2_1']},
      {name: 'f1_1_2', output: 'A1_1', inputs: ['A2_2']},
      {name: 'f1_2_1', output: 'A1_2', inputs: ['A2_1']},
      {name: 'f1_2_2', output: 'A1_2', inputs: ['A2_2']},
      {name: 'f2_1_1', output: 'A2_1', inputs: ['A3_1']},
      {name: 'f2_1_2', output: 'A2_1', inputs: ['A3_2']},
      {name: 'f2_2_1', output: 'A2_2', inputs: ['A3_1']},
      {name: 'f2_2_2', output: 'A2_2', inputs: ['A3_2']},
      {name: 'Fx1', output: 'A3_1', inputs: ['X']},
      {name: 'Fx2', output: 'A3_2', inputs: ['X']}
    ],
    inputs: ['X'],
    output: 'O',
    targetSolution: [
      [
        {name: "Fo1", output: "O", inputs: ["A1_1"]}, 
        {name: "f1_1_1", output: "A1_1", inputs: ["A2_1"]}, 
        {name: "f2_1_1", output: "A2_1", inputs: ["A3_1"]}, 
        {name: "Fx1", output: "A3_1", inputs: ["X"]}
      ], [
        {name: "Fo1", output: "O", inputs: ["A1_1"]}, 
        {name: "f1_1_1", output: "A1_1", inputs: ["A2_1"]}, 
        {name: "f2_1_2", output: "A2_1", inputs: ["A3_2"]}, 
        {name: "Fx2", output: "A3_2", inputs: ["X"]}
      ], [
        {name: "Fo1", output: "O", inputs: ["A1_1"]}, 
        {name: "f1_1_2", output: "A1_1", inputs: ["A2_2"]}, 
        {name: "f2_2_1", output: "A2_2", inputs: ["A3_1"]}, 
        {name: "Fx1", output: "A3_1", inputs: ["X"]}
      ], [
        {name: "Fo1", output: "O", inputs: ["A1_1"]}, 
        {name: "f1_1_2", output: "A1_1", inputs: ["A2_2"]}, 
        {name: "f2_2_2", output: "A2_2", inputs: ["A3_2"]}, 
        {name: "Fx2", output: "A3_2", inputs: ["X"]}
      ], [
        {name: "Fo2", output: "O", inputs: ["A1_2"]}, 
        {name: "f1_2_1", output: "A1_2", inputs: ["A2_1"]}, 
        {name: "f2_1_1", output: "A2_1", inputs: ["A3_1"]}, 
        {name: "Fx1", output: "A3_1", inputs: ["X"]}
      ], [
        {name: "Fo2", output: "O", inputs: ["A1_2"]}, 
        {name: "f1_2_1", output: "A1_2", inputs: ["A2_1"]}, 
        {name: "f2_1_2", output: "A2_1", inputs: ["A3_2"]}, 
        {name: "Fx2", output: "A3_2", inputs: ["X"]}
      ], [
        {name: "Fo2", output: "O", inputs: ["A1_2"]}, 
        {name: "f1_2_2", output: "A1_2", inputs: ["A2_2"]}, 
        {name: "f2_2_1", output: "A2_2", inputs: ["A3_1"]}, 
        {name: "Fx1", output: "A3_1", inputs: ["X"]}
      ], [
        {name: "Fo2", output: "O", inputs: ["A1_2"]}, 
        {name: "f1_2_2", output: "A1_2", inputs: ["A2_2"]}, 
        {name: "f2_2_2", output: "A2_2", inputs: ["A3_2"]}, 
        {name: "Fx2", output: "A3_2", inputs: ["X"]}]]
  
  },
  {
    name: 'multiple inputs, one input can make output',
    sigs: [
      {name: 'f', output: 'O', inputs: ['X', 'Y']},
      {name: 'gx', output: 'O', inputs: ['X']},
      {name: 'gy', output: 'O', inputs: ['Y']}
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "f", output: "O", inputs: ["X", "Y"]}
      ], [
        {name: "gx", output: "O", inputs: ["X"]}
      ], [
        {name: "gy", output: "O", inputs: ["Y"]}
      ]
    ]
  },
  {
    /*
      o
      | \   f  gb
      |  b  f
      | /   f  ga
      a
      |     h
      x
      
    */
    name: 'intermediate node multiple times',
    sigs: [
      {name: 'f', output: 'O', inputs: ['A']},
      {name: 'h', output: 'A', inputs: ['X']},
      {name: 'gb', output: 'O', inputs: ['B']},
      {name: 'ga', output: 'B', inputs: ['A']},
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "f", output: "O", inputs: ["A"]},
        {name: "h", output: "A", inputs: ["X"]}
      ], [
        {name: "gb", output: "O", inputs: ["B"]},
        {name: "ga", output: "B", inputs: ["A"]},
        {name: "h", output: "A", inputs: ["X"]}
      ]
    ]
  },
  {
    /*
        o
        | \      f  g
        |  b     f
        |  |     f  k
        |  ^     f  k
        | / \    f  k
        a    y
        |        h
        x
    */
    name: 'single and two input solution',
    sigs: [
      {name: 'f', output: 'O', inputs: ['A']},
      {name: 'h', output: 'A', inputs: ['X']},
      {name: 'k', output: 'B', inputs: ['A', 'Y']},
      {name: 'g', output: 'O', inputs: ['B']},
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [ // single input
        {name: "f", output: "O", inputs: ["A"]},
        {name: "h", output: "A", inputs: ["X"]}
      ], [ // two input
        {name: "g", output: "O", inputs: ["B"]},
        {name: "k", output: "B", inputs: ["A", "Y"]},
        {name: "h", output: "A", inputs: ["X"]}
      ]
    ]
  },
  {
    /*
        o
        |       f
        ^       f
        | \     f
        |  a    f
        |  |    f  g
        |  ^    f  g
        | / \   f  g
        x   y
        
    */
    name: 'an input twice',
    sigs: [
      {name: 'f', output: 'O', inputs: ['X', 'A']},
      {name: 'g', output: 'A', inputs: ['X', 'Y']},
    ],
    inputs: ['X', 'Y'],
    output: 'O',
    targetSolution: [
      [
        {name: "f", output: "O", inputs: ["X", "A"]},
        {name: "g", output: "A", inputs: ["X", "Y"]}
      ]
    ]
  },
  // with sets:
  {
    /*
     f: (Real * Positive) -> String
     g: (Real) -> Real * Positive
     sln: (Real) -> String
    */
    name: 'intersection',
    sigs: [
      {name: 'f', output: 'String', inputs: [setIntersection('Real', 'Positive')]},
      {name: 'g', output: setIntersection('Real', 'Positive'), inputs: ['Real']},
      {name: 'rh1', output: 'Integer', inputs: [setIntersection('Real', 'Negative')]},
      {name: 'rh2', output: 'String', inputs: ['Vector']},
    ],
    inputs: ['Real'],
    output: 'String',
    targetSolution: [
      [{name: "f", output: "String", inputs: [
        {kind: "intersection", terms: ["Real", "Positive"]}]},
        {name: "g", output: {kind: "intersection", terms: ["Real", "Positive"]}, inputs: ["Real"]}
      ]
    ],
    setsDB: {}
  },
]

for(const {name, sigs, inputs, output, targetSolution, setsDB} of findComposableSigHypertreeTestsDat) {
  addTest(`findComposableSigHypertree ${name}`, () => {
    const foundSolution = [...findComposableSigHypertree(sigs, inputs, output, setsDB ?? {})]
    assertEqualsUnordered(foundSolution, targetSolution)
  })
}

addTest('findComposableSigHypertree', () => {
  const testSigs = [
    {name: 'f', output: 'O', inputs: ['A', 'Y']},
    {name: 'h', output: 'O', inputs: ['B', 'X']},
    {name: 'g', output: 'A', inputs: ['X']},
    {name: 'k', output: 'B', inputs: ['Y']},
    {name: 'ea', output: 'EAO', inputs: ['A']},
    {name: 'ex', output: 'EXO', inputs: ['X']},
    {name: 'ey', output: 'EYO', inputs: ['Y']},
    {name: 'eo', output: 'EOO', inputs: ['O']},
    {name: 'sx', output: 'X', inputs: ['SXI']},
    {name: 'sy', output: 'Y', inputs: ['SYI']},
    {name: 'so', output: 'O', inputs: ['SOI']},
    {name: 'exy', output: 'EXYO', inputs: ['X', 'Y']},
    {name: 'rxy', output: 'Y', inputs: ['X']},
    {name: 'ryx', output: 'X', inputs: ['Y']}
  ]
  const slns = [...findComposableSigHypertree(testSigs, ['X', 'Y'], 'O', {})]
  assertEquals(slns, [
    [{name: "f", output: "O", inputs: ["A", "Y"]}, {name: "g", output: "A", inputs: ["X"]}], 
    [{name: "h", output: "O", inputs: ["B", "X"]}, {name: "k", output: "B", inputs: ["Y"]}]
  ])
})


// ---------------------------------

/*
  `makeCompoundSigFromSigHypertree` takes a collection of signatures and makes a compound sig (a sig composing other sigs)
  Use it on a single yielded output from findComposableSigHypertree
*/
function makeCompoundSigFromSigHypertree(treeSigs, output, setsDB) {
  const retSig = {kind:'frontier', type:output}
  const frontierNodes = [retSig]
  while(frontierNodes.length > 0) {
    const node = frontierNodes.pop()
    assertEquals(node.kind, 'frontier')
    const {kind, type} = node
    
    // find one down-edge (because its a tree), or zero down-edges (then node is considered an input)
    const dnSigsList = sigsDn(treeSigs, [type], setsDB)
    assert(dnSigsList.length === 0 || dnSigsList.length === 1)
    if(dnSigsList.length === 0) { // no sigs found, consider it an input
      node.kind = 'input'
      continue
      
    } else if(dnSigsList.length === 1) { // found one sig
      const dnSig = dnSigsList[0]
      
      // make node look like sig
      delete node.kind
      delete node.type 
      node.name = dnSig.name
      node.output = dnSig.output
      node.inputs = dnSig.inputs.map(input => {
        const newFrontierNode = {kind:'frontier', type:input}
        frontierNodes.push(newFrontierNode)
        return newFrontierNode
      })
      
    } else {
      throw Error(`Multiple downstream edges found in tree for node ${node}`)
    }
  }
  return retSig
}
addTest('makeCompoundSigFromSigHypertree simple', () => {
  assertEquals(
    makeCompoundSigFromSigHypertree([
      {name: 'f', output: 'O', inputs: ['X', 'Y']}
    ], 'O', {}), 
    {name: 'f', output: 'O', inputs: [ { kind: 'input', type: 'X' }, { kind: 'input', type: 'Y' } ] } 
  )
})
addTest('makeCompoundSigFromSigHypertree depth 1', () => {
  assertEquals(
    makeCompoundSigFromSigHypertree([
      {name: 'f', output: 'O', inputs: ['A', 'Y']},
      {name: 'g', output: 'A', inputs: ['X']}
    ], 'O', {}), 
    {name: "f", output: "O", inputs: [
      {name: "g", output: "A", inputs: [{kind: "input", type: "X"}]},
      {kind: "input", type: "Y"}
    ]}
  )
})
addTest('makeCompoundSigFromSigHypertree chain', () => {
  [
      {name: 'Fo', output: 'O', inputs: ['A1']},
      {name: 'f1', output: 'A1', inputs: ['A2']},
      {name: 'f2', output: 'A2', inputs: ['A3']},
      {name: 'Fx', output: 'A3', inputs: ['X']}
    ]
  assertEquals(
    makeCompoundSigFromSigHypertree([
      {name: 'Fo', output: 'O', inputs: ['A1']},
      {name: 'f1', output: 'A1', inputs: ['A2']},
      {name: 'f2', output: 'A2', inputs: ['A3']},
      {name: 'Fx', output: 'A3', inputs: ['X']}
    ], 'O', {}), 
    {name: "Fo", output: "O", inputs: [{name: "f1", output: "A1", inputs: [{name: "f2", output: "A2", inputs: [{name: "Fx", output: "A3", inputs: [{kind: "input", type: "X"}]}]}]}]}
  )
})

// ---------------------------------

/*
  `makeAppsFromCompoundSig` makes compound signatures into app objects
  Like: `{name: 'f', output: 'O', inputs: [{name: ..., output:..., ...}, ...]}`
  Into: `{name: 'f', args: [{name: ..., ...}, ...]}`
  
  Yields application objects that looks like {name: foo, args: [...]}
  by replacing leaf nodes like {kind:'input', type:'MyType'} in `compoundSig`
  with input names by finding the corresponding 'myName' in 
  elements like ['myName', 'MyType'] in `realInputs`
  
  `realInputs` is like [['name', 'type'], ...], and
  `compoundSig` is a nested object with nodes like:
    * {name:'someSigName', output:'someType', inputs: [...]}
    * {kind:'input', type:'someOtherType'}
*/
function* makeAppsFromCompoundSig(compoundSig, realInputs, setsDB) {
  assertEquals((new Set(realInputs.map(x => x.name))).size, realInputs.length, 'Duplicate input names given')
  // realInputs like [{name: ..., type: ...}, ...]
  
  // appTemplate is what we'll create the apps we yield with
  // we copy appTemplate and replace each found input node in it with 
  // realInputs names and then yield it
  const appTemplate = structuredClone(compoundSig)
  
  // find the paths and types of the input nodes we'll replace in appTemplate
  // we'll also collect the template's nodes so we can make the
  // template look like an application instead of a compound signature
  // we can't just change its nodes in this next loop because
  // eachPathAndObject expects immutability
  const appTemplateSigNodes = []
  const inputNodePaths = []
  const inputNodeTypes = []
  for(const [path, node] of eachPathAndObject(appTemplate)) {
    const type = typeof node
    if(type !== 'object')
      continue
    const kind = node?.kind
    if(kind === 'input') {
      inputNodePaths.push(path.slice())
      inputNodeTypes.push(node.type)
    } else if(kind == undefined  &&  !Array.isArray(node)) { // a regular sig node
      // can't modify right here because `eachPathAndObject` expects appTemplate to be immutable
      appTemplateSigNodes.push(node)
    }
  }
  
  // TODO: is this actually an error?
  if(inputNodePaths.length === 0)
    throw Error('No input nodes found')
  
  // now make appTemplate's sig nodes look like app nodes
  for(const sigNode of appTemplateSigNodes) {
    delete sigNode.output
    // rename inputs to args:
    sigNode.args = sigNode.inputs
    delete sigNode.inputs
  }
  
  // find real input indices that match input node types 
  const inputNodesMatchingRealIndices = inputNodeTypes.map(nodeType => {
    const ret = []
    for(let i = 0; i < realInputs.length; i++) {
      const realInputType = realInputs[i].type
      if(isSubset(realInputType, nodeType, setsDB))
        ret.push(i)
    }
    return ret
  })
  // was a real input found for each input node?
  for(const indices of inputNodesMatchingRealIndices) {
    if(indices.length > 0)
      continue
    // else, input node has no corresponding given real input, so can't yield any apps
    return // no apps possible
  }
  // inputNodesMatchingRealIndices now like eg: [[1, 2, 3], [2, 4], ...]
  // where elems are lists of which realInputs indices match that inputNode's type
  
  // BUG: this loop should use all combinations
  for(const combination of generateAllCombinations(inputNodesMatchingRealIndices)) {
    const app = structuredClone(appTemplate)
    for(let i = 0; i < inputNodePaths.length; i++) {
      // get input nodes's parent's key, and input node's key in parent
      const nodePath = inputNodePaths[i]
      const parentPath = nodePath.slice(0, -1) // slice off end
      const nodeKey = last(nodePath)
      // traverse to parent node
      let inputParent = app
      for(const key of parentPath)
        inputParent = inputParent[key === 'inputs' ? 'args' : key]
      // replace input node in parent with real input name
      const realInputI = combination[i]
      inputParent[nodeKey] = realInputs[realInputI].name
    }
    yield app
  }
}
addTest('makeAppsFromCompoundSig', () => {
  const sig = {
    name:'f', output:'O', inputs:[
      {kind:'input', type:'X'},
      {kind:'input', type:'Y'}
    ]
  }
  const res = [...makeAppsFromCompoundSig(sig, [{name:'x', type:'X'}, {name:'y', type:'Y'}], {})]
  assertEquals(res, [{name: "f", args: ["x", "y"]}])
})

// ---------------------------------

/*
  `findApps` chains `findComposableSigHypertree`, `makeCompoundSigFromSigHypertree` and `makeAppsFromCompoundSig` in the obvious way
  Yields the return of `makeAppsFromCompoundSig`
*/
function* findApps(allSigs, realInputs, targetOutput, setsDB) {
  const targetInputs = realInputs.map(x => x.type)
  for(const solution of findComposableSigHypertree(allSigs, targetInputs, targetOutput, setsDB)) {
    const compoundSig = makeCompoundSigFromSigHypertree(solution, targetOutput, setsDB)
    for(const app of makeAppsFromCompoundSig(compoundSig, realInputs, setsDB))
      yield app
  }
}

const findAppsTestsDat = [
  // Re-using findComposableSigHypertree tests because I'm lazy
  {
    name: 'sigs1',
    sigs: [
      {name: 'f', output: 'A', inputs: ['X']},
      {name: 'g', output: 'O', inputs: ['A', 'Y']},
      {name: 'ha', output: 'B', inputs: ['A']},
      {name: 'ho', output: 'C1', inputs: ['O']},
      {name: 'hc', output: 'O', inputs: ['C2']},
      {name: 'hy', output: 'C3', inputs: ['Y']},
      {name: 'hc2', output: 'Y', inputs: ['C4']}
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [{name: "g", args: [{name: "f", args: ["x"]}, "y"]}]
  },
  {
    name: 'sigs2',
    sigs: [
      {name: 'f', output: 'C', inputs: ['O', 'B']},
      {name: 'g', output: 'O', inputs: ['X', 'Y']},
      {name: 'h', output: 'A', inputs: ['Z']},
      {name: 'F', output: 'O', inputs: ['A']},
      {name: 'G', output: 'D', inputs: ['A']},
      {name: 'H', output: 'A', inputs: ['E']}
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [{name: "g", args: ["x", "y"]}]
  },
  {
    name: 'multiple inputs, multiple solutions',
    sigs: [
      {name: 'f', output: 'A', inputs: ['X']},
      {name: 'g', output: 'O', inputs: ['A', 'Y']},
      {name: 'h', output: 'B', inputs: ['Y']},
      {name: 'j', output: 'O', inputs: ['X', 'B']},
      {name: 'F', output: 'C', inputs: ['O']},
      {name: 'G', output: 'D', inputs: ['A']},
      {name: 'H', output: 'E', inputs: ['Y']}
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [
      {name: "g", args: [{name: "f", args: ["x"]}, "y"]},
      {name: "j", args: ["x", {name: "h", args: ["y"]}]}
    ]
  },
  {
    name: 'sigs4',
    sigs: [
      {name: 'g', output: 'O', inputs: ['X', 'Y', 'Z']},
      {name: 'f', output: 'O', inputs: ['A', 'B']},
      {name: 'ha', output: 'A', inputs: ['X']},
      {name: 'hb', output: 'B', inputs: ['Y']}
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [{name: "f", args: [{name: "ha", args: ["x"]}, {name: "hb", args: ["y"]}]}]
  },
  {
    name: 'sigs5',
    sigs: [
      {name: 'f', output: 'O', inputs: ['X', 'A']},
      {name: 'g', output: 'A', inputs: ['B']},
      {name: 'h', output: 'B', inputs: ['Y']},
      {name: 'j', output: 'X', inputs: ['Y']},
      {name: 'fux', output: 'UX', inputs: ['X']},
      {name: 'fub', output: 'UB', inputs: ['B']},
      {name: 'fua', output: 'UA', inputs: ['A']},
      {name: 'fuy', output: 'UY', inputs: ['Y']},
      {name: 'fuo', output: 'UO', inputs: ['O']},
      {name: 'fdx', output: 'X', inputs: ['DX']},
      {name: 'fdb', output: 'B', inputs: ['DB']},
      {name: 'fda', output: 'A', inputs: ['DA']},
      {name: 'fdy', output: 'Y', inputs: ['DY']},
      {name: 'fdo', output: 'O', inputs: ['DO']}
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [{name: "f", args: ["x", {name: "g", args: [{name: "h", args: ["y"]}]}]}]
  },
  {
    name: 'chain length 3',
    sigs: [
      {name: 'Fo', output: 'O', inputs: ['A1']},
      {name: 'f1', output: 'A1', inputs: ['A2']},
      {name: 'f2', output: 'A2', inputs: ['A3']},
      {name: 'Fx', output: 'A3', inputs: ['X']}
    ],
    realInputs: [{name:'x', type:'X'}],
    output: 'O',
    targetSolutions: [{name: "Fo", args: [{name: "f1", args: [{name: "f2", args: [{name: "Fx", args: ["x"]}]}]}]}]
  },
  {
    /*
      This problem's hypertree looks like
        o
      /  \   Fo1  Fo2
     | X |   f1_1_1  f1_1_2  f1_2_1  f1_2_2
     | X |   f2_1_1  f2_1_2  f2_2_1  f2_2_2
     \  /    Fx1  Fx2
      x
     Where the `fN_M_K` edges stand for unary sigs between non-terminal-connected layers
     The `FoN` edges connect to the output
     The `FxN` edges connect to the input
    */
    name: 'ladder width 2 length 3',
    sigs: [
      {name: 'Fo1', output: 'O', inputs: ['A1_1']},
      {name: 'Fo2', output: 'O', inputs: ['A1_2']},
      {name: 'f1_1_1', output: 'A1_1', inputs: ['A2_1']},
      {name: 'f1_1_2', output: 'A1_1', inputs: ['A2_2']},
      {name: 'f1_2_1', output: 'A1_2', inputs: ['A2_1']},
      {name: 'f1_2_2', output: 'A1_2', inputs: ['A2_2']},
      {name: 'f2_1_1', output: 'A2_1', inputs: ['A3_1']},
      {name: 'f2_1_2', output: 'A2_1', inputs: ['A3_2']},
      {name: 'f2_2_1', output: 'A2_2', inputs: ['A3_1']},
      {name: 'f2_2_2', output: 'A2_2', inputs: ['A3_2']},
      {name: 'Fx1', output: 'A3_1', inputs: ['X']},
      {name: 'Fx2', output: 'A3_2', inputs: ['X']}
    ],
    realInputs: [{name:'x', type:'X'}],
    output: 'O',
    targetSolutions: [
      {name: "Fo1", args: [{name: "f1_1_1", args: [{name: "f2_1_1", args: [{name: "Fx1", args: ["x"]}]}]}]}, 
      {name: "Fo1", args: [{name: "f1_1_1", args: [{name: "f2_1_2", args: [{name: "Fx2", args: ["x"]}]}]}]}, 
      {name: "Fo1", args: [{name: "f1_1_2", args: [{name: "f2_2_1", args: [{name: "Fx1", args: ["x"]}]}]}]}, 
      {name: "Fo1", args: [{name: "f1_1_2", args: [{name: "f2_2_2", args: [{name: "Fx2", args: ["x"]}]}]}]}, 
      {name: "Fo2", args: [{name: "f1_2_1", args: [{name: "f2_1_1", args: [{name: "Fx1", args: ["x"]}]}]}]}, 
      {name: "Fo2", args: [{name: "f1_2_1", args: [{name: "f2_1_2", args: [{name: "Fx2", args: ["x"]}]}]}]}, 
      {name: "Fo2", args: [{name: "f1_2_2", args: [{name: "f2_2_1", args: [{name: "Fx1", args: ["x"]}]}]}]}, 
      {name: "Fo2", args: [{name: "f1_2_2", args: [{name: "f2_2_2", args: [{name: "Fx2", args: ["x"]}]}]}]}
    ]

  
  },
  {
    name: 'multiple inputs, one input can make output',
    sigs: [
      {name: 'f', output: 'O', inputs: ['X', 'Y']},
      {name: 'gx', output: 'O', inputs: ['X']},
      {name: 'gy', output: 'O', inputs: ['Y']}
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [
      {name: "f", args: ["x", "y"]},
      {name: "gx", args: ["x"]},
      {name: "gy", args: ["y"]}
    ]
  },
  {
    /*
      o
      | \   f  gb
      |  b  f
      | /   f  ga
      a
      |     h
      x
      
    */
    name: 'intermediate node multiple times',
    sigs: [
      {name: 'f', output: 'O', inputs: ['A']},
      {name: 'h', output: 'A', inputs: ['X']},
      {name: 'gb', output: 'O', inputs: ['B']},
      {name: 'ga', output: 'B', inputs: ['A']},
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [
      {name: "f", args: [{name: "h", args: ["x"]}]},
      {name: "gb", args: [{name: "ga", args: [{name: "h", args: ["x"]}]}]}
    ]
  },
  {
    /*
        o
        | \      f  g
        |  b     f
        |  |     f  k
        |  ^     f  k
        | / \    f  k
        a    y
        |        h
        x
    */
    name: 'single and two input solution',
    sigs: [
      {name: 'f', output: 'O', inputs: ['A']},
      {name: 'h', output: 'A', inputs: ['X']},
      {name: 'k', output: 'B', inputs: ['A', 'Y']},
      {name: 'g', output: 'O', inputs: ['B']},
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [
      {name: "f", args: [{name: "h", args: ["x"]}]},
      {name: "g", args: [{name: "k", args: [{name: "h", args: ["x"]}, "y"]}]}
    ]
  },
  {
    /*
        o
        |       f
        ^       f
        | \     f
        |  a    f
        |  |    f  g
        |  ^    f  g
        | / \   f  g
        x   y
        
    */
    name: 'an input twice',
    sigs: [
      {name: 'f', output: 'O', inputs: ['X', 'A']},
      {name: 'g', output: 'A', inputs: ['X', 'Y']},
    ],
    realInputs: [{name:'x', type:'X'}, {name:'y', type:'Y'}],
    output: 'O',
    targetSolutions: [{name: "f", args: ["x", {name: "g", args: ["x", "y"]}]}]
  },
  {
    /*
        o
        |    f
        ^    f
       / \   f
      X   X
    */
    name: 'one input type, two input names',
    sigs: [
      {name: 'f', output: 'O', inputs: ['X', 'X']}
    ],
    realInputs: [{name:'a', type:'X'}, {name:'b', type:'X'}],
    output: 'O',
    targetSolutions: [
      {name: "f", args: ["a", "a"]},
      {name: "f", args: ["b", "a"]},
      {name: "f", args: ["a", "b"]},
      {name: "f", args: ["b", "b"]}
    ]

  }
]
for(const {name, sigs, realInputs, output, targetSolutions} of findAppsTestsDat) {
  addTest(`findApps ${name}`, () => {
    const foundSolutions = [...findApps(sigs, realInputs, output, {})]
    assertEqualsUnordered(foundSolutions, targetSolutions)
  })
}

// `getAppInputNames` returns an array of all terminal input arguments in a given app
function getAppInputNames(app) {
  const ret = []
  for(const [path, obj] of eachPathAndObject(app) ) {
    if(path.length === 0  ||  typeof obj !== 'string'  ||  path[path.length - 2] !== 'args')
      continue
    ret.push(obj)
  }
  return ret
}
/*
 `getAppFunctionMap` returns a map (/ object) of all function names used in `app` to their function implementation, like: {name1: func1, name2: func2, ...}
 This requires `allSigs`'s sigs to have `func` properties with the signature's implementation
*/
function getAppFunctionMap(allSigs, app) {
  const ret = {}
  const sigMap = {}
  for(const sig of allSigs)
    sigMap[sig.name] = sig
  for(const [path, obj] of eachPathAndObject(app)) {
    if(path.length === 0  ||  path[path.length - 1] != 'name')
      continue
    const sig = sigMap[obj]
    if(sig == undefined  ||  !('func' in sig))
      continue
    ret[obj] = sig.func
  }
  return ret
}


// ---------------------------------

/*
  `appToJsString` turns an app into a javascript string
  eg: {name: "g", args: [{name: "f", args: ["x"]}, "y"]} -> 'g(f(x), y)'
  For use in eval to turn an app into a real, callable function
*/
function appToJsString(app) {
  // app like {name: "g", args: [{name: "f", args: ["x"]}, "y"]}
  if(typeof app === 'string')
    return app
  assert(typeof app === 'object')
  assert('name' in app)
  assert('args' in app)
  const segs = [app.name, '(']
  
  const stateStack = [[app.args, 0]]
  // stateStack is a stack of [argumentArray, nextArgIndex]
  while(stateStack.length > 0) {
    const state = last(stateStack)
    const [array, i] = last(stateStack)
    
    // done, go up a level
    if(i >= array.length) {
      stateStack.pop()
      segs.push(')')
      continue
    }
    // else, not done
    
    state[1]++ // increment arg index for next pass
    const value = array[i]
    if(i > 0)
      segs.push(', ')
    
    
    // its an input arg
    if(typeof value === 'string') {
      segs.push(value)
      continue
    }
    
    // its another another app
    // go into that app
    assert(typeof value === 'object')
    assert('name' in value)
    assert('args' in value)
    segs.push(value.name, '(')
    stateStack.push([value.args, 0])
    
  }
  
  return segs.join('')
}
addTest('appToJsString', () => {
  assertEquals(appToJsString({name: "g", args: [{name: "f", args: ["x", "y"]}, "z"]}), 'g(f(x, y), z)')
  assertEquals(appToJsString({name: "g", args: [{name: "f", args: []}]}), 'g(f())')
})

// `appToFunction` makes an app object into a callable function
function appToFunction(app, functionMap, inputNames) {
  let appString = `return ${appToJsString(app)}`
  // eval evals in global scope, so we have to have some way to inject our functions from functionMap
  // replace all functions `foo(` with `this.foo(`
  appString = appString.replaceAll(/(\w+)(?=\()/g, "this.$1")
  // bind the functions map so the `this.foo` refers to the right functions and return
  return Function(...inputNames, appString).bind(functionMap)
}
addTest('appToFunction', () => {
  const app = {name: "times2", args: [{name: "add1", args: ["x"]}]}
  const fn = appToFunction(app, {add1:(x)=>x+1, times2:(x)=>x*2}, ['x'])
  assertEquals(fn(5), 12)
  assertEquals(fn(6), 14)
})

// ---------------------------------

/*
  `findFunctions` chains all of the above functions to yield callable functions from a list of inputs, a target output, and all the possible signatures (w/ attached implementations) it can compose
  
  `allSigs`: [SIG, SIG, ...]
  `realInputs`: [{name:'someName', type:TYPE}, ...]
  `targetOutput`: TYPE
  `setsDB`: Optional; see comment above `isAtomicSet`, like: {'SomeSet': {'SomeSubset': true, ...}, ...}
    Where 
      SIG is a function signature like {name: 'foo', output: TYPE, inputs: [TYPE, ...], func: FUNC}
      Where TYPE is the string name of a type (eg: `Number`, `String`, etc), or a type intersection object produced by `setIntersection`, or a type union object produced by `setUnion`
      And FUNC is the implementation of the signature to be used in composition
  
  Note: don't construct `setsDB` manually, use `addSubsetRelation(superset, subset, setsDB)` to add a superset-subset relationship to `setsDB`
*/
function* findFunctions(allSigs, realInputs, targetOutput, setsDB = {}, appFilter = truePred) {
  for(const app of findApps(allSigs, realInputs, targetOutput, setsDB)) {
    if(!appFilter(app))
      continue
    yield appToFunction(app, getAppFunctionMap(allSigs, app), getAppInputNames(app))
  }
}
addTest('findFunctions', () => {
  const sigs = [
    {name: 'times2', output: 'O', inputs: ['Y'], func:(x)=>x*2},
    {name: 'add1', output: 'Y', inputs: ['X'], func:(x)=>x+1}
  ]
  const fns = [...findFunctions(sigs, [{name:'x', type:'X'}], 'O', {})]
  assertEquals(fns.length, 1)
  assertEquals(fns[0](4), 10)
})


// ---------------------------------

runTests()
print('Done')

