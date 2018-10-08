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
            //do Thread.Sleep(1)
        else ()

    let private calcCostOfPath origin (path: SimpleWorld.Position list) =
        let rec c t (l: SimpleWorld.Position) (r: SimpleWorld.Position list) =
            match r with
            | [] -> t
            | _ -> c (t + l.costTo r.Head) r.Head r.Tail
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
        Display.printNodes() (Map.toList world.Positions |> List.map (fun (k, v) -> v))
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
        match Solver.solve t with
        | {Status = Unsolveable} -> Display.displayError() "Unsolveable"; None
        | {Status = TickLimitReached} -> Display.displayError() "Tick Limit Reached"; None
        | {Status = Solved} as s ->
            for n in s.ClosedNodes do Display.printNode() n
            Some s
        | _ -> raise (Exception "How did I get here")

    let solveSolvers instance =
        {
            instance with Results = List.map (solve instance) instance.Results |> List.where (fun t -> t.IsSome) |> List.map (fun t -> t.Value)
        }

    let printResult instance offset thoroughness (result: Solver<SimpleWorld.Position>) isShowing =
        Console.CursorTop <- offset
        Console.CursorLeft <- 0
        Console.BackgroundColor <- if isShowing then ConsoleColor.Cyan else ConsoleColor.Black
        Console.ForegroundColor <- ConsoleColor.White
        Console.Write("                 ")
        Console.CursorLeft <- 0
        Console.Write ((string thoroughness) + "\t" + (string (calcCostOfPath instance.Origin result.Path |> int)) + "\t" + (string (result.Ticks)))

    let promptResults instance =
        let rec k last next =
            let mutable i = 0
            for result in instance.Results do
                printResult instance i instance.Thoroughnesses.[i] result (i = next)
                i <- i + 1
            for n in instance.Results.[last].Path do Display.printNode() n
            Display.printPath() instance.Results.[next].Path
            Display.printMessage() "0-9 for path, Enter for next map, Q to quit"
            Display.debugEndPointNode() instance.Origin
            Display.debugEndPointNode() instance.Destination
            match Console.ReadKey(true) with
            | ky when ky.Key = ConsoleKey.Enter -> true
            | ky when ky.Key = ConsoleKey.Q -> false
            | ky when (isNumInput ky) ->
                let n = ky.KeyChar |> string |> int
                if n >=0 && n < instance.Results.Length then k next n
                else k last next
            | _ -> k last next
        k 0 0

    [<EntryPoint>]
    let main argv =
        Console.CursorVisible <- false
        Console.ForegroundColor <- ConsoleColor.White
        Console.BackgroundColor <- ConsoleColor.Black
        Console.Clear()

        let rec run = function 
            | true -> createInstance() |> createSolvers |> solveSolvers |> promptResults |> run
            | false -> false
        run true |> ignore
        0

