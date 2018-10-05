// Learn more about F# at http://fsharp.org

namespace FAStarInteractiveTest
open System
open System.Threading
open FAStar
open SimpleWorld

type WorldInstance =
    {
        World: SimpleWorld.World
        Origin: SimpleWorld.Position
        Destination: SimpleWorld.Position
        Thoroughnesses: float list
        Results: Solver<SimpleWorld.Position> list
    }

module Main =
    let doDebug = true

    let private debug(solver: Solver<SimpleWorld.Position>) =
        if doDebug && not (solver.CurrentNode = solver.OriginNode) && not (solver.CurrentNode = solver.DestinationNode) then
            do Display.debugCurrentNode() solver.CurrentNode
        else ()

    let private calcCostOfPath origin (path: SimpleWorld.Position list) =
        let rec c t (l: SimpleWorld.Position) (r: SimpleWorld.Position list) =
            match r with
            | [] -> t
            | _ -> c (t + r.Head.costTo l) r.Head r.Tail
        c 0.0 origin path

    let private isNumInput (consoleKeyInfo: ConsoleKeyInfo) =
        List.contains  consoleKeyInfo.KeyChar ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0']

    let createInstance() =
        let world = SimpleWorld.create Console.WindowWidth (Console.WindowHeight)
        let random = new Random()
        let rec chooseRandomNode() =
            match world.Positions.[random.Next(world.Positions.Count - 1)] with
            | p when not (p.Type = SimpleWorld.Wall) -> p
            | _ -> chooseRandomNode()
        {
            World = world
            Origin = chooseRandomNode()
            Destination = chooseRandomNode()
            Thoroughnesses = [0.0; 0.1; 0.2; 0.3; 0.35; 0.4; 0.45; 0.5; 1.0]
            Results = List.empty
        }

    let createSolver origin destination thoroughness (world: SimpleWorld.World)=
            {
                Solver.create origin destination (world.neighbors) (fun f t -> f.costTo t) (fun f t -> f.distanceTo t) with
                    Iter = (debug)
                    Thoroughness = thoroughness
            }

    let createSolvers instance =
        let cs t = createSolver instance.Origin instance.Destination t instance.World
        {
            instance with Results = List.map (cs) instance.Thoroughnesses
        }

    let solve instance t =
        Display.printMessage() (t.Thoroughness.ToString())
        Display.debugEndPointNode() instance.Origin
        Display.debugEndPointNode() instance.Destination
        let solver = Solver.solve t
        match solver.Status with
            | Unsolveable -> do Display.displayError() "Unsolveable"
            | TickLimitReached -> do Display.displayError() "Tick Limit Reached"
            | Solved ->
                Display.printPath() solver.path
            | _ -> raise (Exception "How did I get here")
        for n in solver.ClosedNodes do Display.printNode() n
        solver

    let solveSolvers instance =
        Display.printNodes() (Map.toList instance.World.Positions |> List.map (fun (k, v) -> v))
        {
            instance with Results = List.map (solve instance) instance.Results
        }

    let printResult instance offset thoroughness (result: Solver<SimpleWorld.Position>) isShowing =
        Console.CursorTop <- offset
        Console.CursorLeft <- 0
        Console.BackgroundColor <- if isShowing then ConsoleColor.Cyan else ConsoleColor.Black
        Console.ForegroundColor <- ConsoleColor.White
        Console.Write("                 ")
        Console.CursorLeft <- 0
        Console.Write ((string thoroughness) + "\t" + (string (calcCostOfPath instance.Origin result.path |> int)) + "\t" + (string (result.Ticks)))

    let promptResults instance =
        let rec k last next =
            let mutable i = 0
            for result in instance.Results do
                printResult instance i instance.Thoroughnesses.[i] result (i = next)
                i <- i + 1
            for n in instance.Results.[last].path do Display.printNode() n
            Display.printPath() instance.Results.[next].path
            match Console.ReadKey(true) with
            | ky when ky.Key = ConsoleKey.Enter || ky.Key = ConsoleKey.Q -> ()
            | ky when (isNumInput ky) ->
                let n = ky.KeyChar |> string |> int
                if n >=0 && n < instance.Results.Length then k next n
                else k last next
            | _ -> k last next
        k 0 0

    [<EntryPoint>]
    let main argv =
        Console.ForegroundColor <- ConsoleColor.White
        Console.BackgroundColor <- ConsoleColor.Black
        Console.Clear()
        while Display.promptToContinue() do
            createInstance() |> createSolvers |> solveSolvers |> promptResults
        0
