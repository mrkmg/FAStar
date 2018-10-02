// Learn more about F# at http://fsharp.org

namespace FAStarInteractiveTest

module Main =
     open System
     open System.Threading
     open FAStar
     open SimpleWorld

     let debug(state: Solver.State<SimpleWorld.Position>) =
        if not (state.LastNode = state.OriginNode) && not (state.LastNode = state.DestinationNode) then
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
                Display.debugEndPointNode() origin
                Display.debugEndPointNode() destination
                let state = Solver.solve (createSolver origin destination i world)
                Display.printPath() state.path
                listOfPaths <- List.append listOfPaths [state]
                for n in state.ClosedNodes do Display.printNode() n

            let mutable lastPrintedIndex = 0
            let printPath index =
                if index >= 0 && index < listOfPaths.Length then
                    let totalCost = listOfPaths.[index].path |> List.map (fun t -> t.travelCost) |> List.sum
                    let totalTicks = listOfPaths.[index].Ticks
                    for n in listOfPaths.[lastPrintedIndex].path do Display.printNode() n
                    Display.printMessage() ((string thoroughnesses.[index]) + " " + (string totalCost) + " " + (string totalTicks))
                    Display.printPath() listOfPaths.[index].path
                    Display.debugEndPointNode() origin
                    Display.debugEndPointNode() destination
                    lastPrintedIndex <- index

            let mutable cont = true
            Display.printMessage() "Choose a path to view"
            while cont do
                match Display.askForSingleKey() with
                    | k when k.Key = ConsoleKey.Enter || k.Key = ConsoleKey.Q -> cont <- false
                    | k when (isNumInput k) -> printPath (k.KeyChar |> string |> int)
                    | _ -> Display.printMessage() "Unknown Path"
         with
             | Solver.Unsolveable -> Display.displayError() "Unsolveable"
             | Solver.MaxTickReached -> Display.displayError() "Max Tick Limit Reached"


     [<EntryPoint>]
     let main argv =
         Console.ForegroundColor <- ConsoleColor.White
         Console.BackgroundColor <- ConsoleColor.Black
         Console.Clear()
         while Display.promptToContinue() do
             genAndPrintMap()
         0
