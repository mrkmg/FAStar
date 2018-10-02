# FAStar

FAStar is a generic A* graph path finding solver written in F#

## Quick Start

To find a path between two nodes in a graph, you need to have the following:

Assume that the type of your nodes is `'T`

- Origin and Destination Nodes
- An expression to get the neighbors of a given node
	+ `'T -> List<'T>`
- An expression to get the cost from move from one node to another which returns a double
	+ `'T -> 'T -> double`
- An expression to get the estimated cost from one node to another which returns a double
	+ `'T -> 'T -> double`
	
Once you have those requirements, you can create a state

```fsharp
let initialState = FAStar.Solver.create originNode destinationNode (getNeighbors) (getCost) (estimateCost)
```

You can also override some settings of the solver state.

```fsharp
let initialState = {
	FAStar.Solver.create originNode destinationNode (getNeighbors) (getCost) (estimateCost) 
} with
	Thoroughness = 0.3
	MaxTicks = 4000
```

Once you have a Solver.State instance, you can run it through the solver to get a solved state back. Be sure to catch for errors.

```fsharp
try
	let solvedState = FAStar.Solver.solve initialState
with
	| Unsolvable -> Console.WriteLine "There is no path"
	| MaxTickReached -> Console.WriteLine "Could not find a path in the allotted ticks"
```

Finally, with a solved state, you can get the path from the origin node to the destination node

```fsharp
let path = solvedState.path
```


## Documentation

### Solver.State<'T when 'T : comparison>

A Type

The `Solver.State<'T>` represents the state of the A* solver. You can set a few items on the solver in order to tailor the A* solver to your needs.

- **Thoroughness**: `float`
	+ This is a factor from 0 to 1 of how thorough the search is. A thoroughness of 0 is the least thorough and equivalent to a "Greedy First" search. A thoroughness of 1 is the most thorough and equivalent to Dijkstra (Breadth-first) search. Values below 0 or above 1 will lead to undefined results.
- **MaxTicks**: `int`
	+ The maximum number of nodes to check before giving up. This is useful if you want to prevent a search from taking too long. By default it is `Int32.MaxValue`, so be careful with very large graphs.
- **Iter**: `Solver.State<'T> -> unit`
	+ A callback function which is executed after each tick of the solver. This is useful for debugging.
	
`Solver.State<'T>` also has some members you to get information about the current state.

- **path**: `List<'T>`
	+ The completed path. Will raise `Solver.NotPathable` is the state is not solved.
- **isSolved**: `bool`
	+ True if the current state is solved.
- **isUnsolvable**: `bool`
	+ True if the current state is unsolvable.
	
### Solver.create

An expression: `'T -> 'T -> ( 'T -> List<'Node>) -> ('T -> 'T -> double) -> ('T -> 'T -> double) -> Solver.State<'T>`

The `Solver.create` function will create a new Solver State. It takes in an origin, destination, getNeighborsFunction, calcCostFunction, and estimateCostFunction. It will return a `Solver.State` which is ready to be solved.

### Solver.tick

An expression: `Solver.State<'T> -> Solver.State<'T>`

This will tick the solver and check the next node. Could raise `Solver.MaxTickReached`, `Solver.Unsolveable`, or `Solver.AlreadySolved`.

### Solver.solve

An expression: `Solver.State<'T> -> Solve.State<'T>`

This will tick the solver until it's solved and return the solved state. Could raise `Solver.MaxTickReached` or `Solver.Unsolveable`.


## License

Copyright 2018 Kevin Gravier <kevin@mrkmg.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.