// Learn more about F# at http://fsharp.org

namespace FAStarInteractiveTest

module Main =
     open System
     open System.Threading
     open FAStar
     open SimpleWorld

     let debug(state: Solver.State<SimpleWorld.Position>) =
             Display.debugCurrentNode() state.LastNode

     let createSolver origin destination thoroughness (world: SimpleWorld.World)=
         {
             Solver.create origin destination (world.neighbors) (fun t -> t.costTo) (fun t -> t.distanceTo) with
                 Iter = (debug)
                 Thoroughness = thoroughness
         }

     let private isNumInput (consoleKeyInfo: ConsoleKeyInfo) =
        List.contains  consoleKeyInfo.KeyChar ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0']

     let genAndPrintMap() =
         try
            let world = SimpleWorld.create Console.WindowWidth (Console.WindowHeight - 3)
            let random = new Random()
            let chooseRandomNode() = world.Positions.[random.Next(world.Positions.Length - 1)]
            let origin = chooseRandomNode()
            let destination = chooseRandomNode()

            let mutable listOfPaths = List.empty
            let thoroughnesses = List.init 7 (fun n -> 0.2 + 0.05 * (float n))

            Display.printNodes() world.Positions
            for i in thoroughnesses do
                Display.printMessage() (i.ToString())
                let state = Solver.solve (createSolver origin destination i world)
                Display.printPath() state.path
                listOfPaths <- List.append listOfPaths [state.path]
                for n in state.ClosedNodes do Display.printNode() n

            Display.printNodes() world.Positions

            let mutable lastPrintedIndex = 0
            let printPath index =
                if index >= 0 && index < listOfPaths.Length then
                    let totalCost = listOfPaths.[index] |> List.map (fun t -> t.travelCost) |> List.sum
                    for n in listOfPaths.[lastPrintedIndex] do Display.printNode() n
                    Display.printMessage() ((string thoroughnesses.[index]) + " " + (string totalCost))
                    Display.printPath() listOfPaths.[index]
                    lastPrintedIndex <- index

            let mutable cont = true
            Display.printMessage() "Choose a path to view"
            while cont do
                match Display.askForSingleKey() with
                    | k when k.Key = ConsoleKey.Enter || k.Key = ConsoleKey.Q -> cont <- false
                    | k when (isNumInput k) -> printPath (k.KeyChar |> string |> int)
                    | _ -> Display.printMessage() "Unknown Path"
         with
             | Solver.Unsolveable msg -> Display.displayError() msg


     [<EntryPoint>]
     let main argv =
         Console.ForegroundColor <- ConsoleColor.White
         Console.BackgroundColor <- ConsoleColor.Black
         Console.Clear()
         while Display.promptToContinue() do
             genAndPrintMap()
         0
