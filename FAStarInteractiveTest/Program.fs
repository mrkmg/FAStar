// Learn more about F# at http://fsharp.org

namespace FAStarInteractiveTest
open System
open System.Threading
open FAStar
open SimpleWorld

module Main =
    let private debug(solver: Solver<SimpleWorld.Position>) =
        ()
//        if not (solver.CurrentNode = solver.OriginNode) && not (solver.CurrentNode = solver.DestinationNode) then
//            do Display.debugCurrentNode() solver.CurrentNode

    let private createSolver origin destination thoroughness (world: SimpleWorld.World)=
        {
            Solver.create origin destination (world.neighbors) (fun t -> t.costTo) (fun t -> t.distanceTo) with
                Iter = (debug)
                Thoroughness = thoroughness
        }
    let private isNumInput (consoleKeyInfo: ConsoleKeyInfo) =
        List.contains  consoleKeyInfo.KeyChar ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0']

    let private calcCostOfPath (path: SimpleWorld.Position list) =
        let rec c t (l: SimpleWorld.Position) r =
            match r with
            | [] -> t
            | _ -> c (t + l.costTo r.Head) r.Head r.Tail
        c 0.0 path.Head path.Tail

    let private printResult offset thoroughness (result: Solver<SimpleWorld.Position>) isShowing =
        Console.CursorTop <- offset
        Console.CursorLeft <- 0
        Console.BackgroundColor <- if isShowing then ConsoleColor.Cyan else ConsoleColor.Black
        Console.ForegroundColor <- ConsoleColor.White
        Console.Write("                                          ")
        Console.CursorLeft <- 0
        Console.Write ((string thoroughness) + "\t" + (string (calcCostOfPath result.path |> int)) + "\t" + (string (result.Ticks)))

    let private showResults (thoroughnesses: float list) (results: Solver<SimpleWorld.Position> list) =
        let rec k last next =
            let mutable i = 0
            for result in results do
                printResult i thoroughnesses.[i] result (i = next)
                i <- i + 1
            for n in results.[last].path do Display.printNode() n
            Display.printPath() results.[next].path
            match Console.ReadKey() with
            | ky when ky.Key = ConsoleKey.Enter || ky.Key = ConsoleKey.Q -> ()
            | ky when (isNumInput ky) ->
                let n = ky.KeyChar |> string |> int
                if n >=0 && n < results.Length then k next n
                else k last next
            | _ -> k last next

        k 0 0

    let genAndPrintMap() =
        let world = SimpleWorld.create Console.WindowWidth (Console.WindowHeight)
        let random = new Random()

        let rec chooseRandomNode() =
            match world.Positions.[random.Next(world.Positions.Count - 1)] with
            | p when not (p.Type = SimpleWorld.Wall) -> p
            | _ -> chooseRandomNode()

        let origin = chooseRandomNode()
        let destination = chooseRandomNode()
        let mutable listOfPaths = List.empty
        let thoroughnesses = [0.0; 0.1; 0.2; 0.3; 0.35; 0.4; 0.45; 0.5;]

        Display.printNodes() (Map.toList world.Positions |> List.map (fun (k, v) -> v))

        for i in thoroughnesses do
            Display.printMessage() (i.ToString())
            Display.debugEndPointNode() origin
            Display.debugEndPointNode() destination
            let solver = Solver.solve (createSolver origin destination i world)
            match solver.Status with
                | Unsolveable -> do Display.displayError() "Unsolveable"
                | TickLimitReached -> do Display.displayError() "Tick Limit Reached"
                | Solved -> 
                    Display.printPath() solver.path
                    listOfPaths <- List.append listOfPaths [solver]
                | _ -> raise (Exception "How did I get here")
            for n in solver.ClosedNodes do Display.printNode() n

        showResults thoroughnesses listOfPaths

    [<EntryPoint>]
    let main argv =
        Console.ForegroundColor <- ConsoleColor.White
        Console.BackgroundColor <- ConsoleColor.Black
        Console.Clear()
        while Display.promptToContinue() do
            genAndPrintMap()
        0
