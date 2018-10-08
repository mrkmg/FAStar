// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Linq
open System.Threading.Tasks
open SimpleWorld
open FAStar

type WorldInstance =
    {
        World: SimpleWorld.World
        Origin: SimpleWorld.Position
        Destination: SimpleWorld.Position
        Thoroughnesses: float list
        Results: Solver<SimpleWorld.Position> list
    }

type SUType = Init | Start | Finish

let getXyForI i =
    let width = Console.WindowWidth / 4
    ((i % width) * 4, i / width)

let cMon = new Object()
let su i m =
    lock cMon (fun () -> 
        let (x, y) = getXyForI i
        Console.CursorTop <- y
        Console.CursorLeft <- x
        match m with
            | Init -> Console.BackgroundColor <- ConsoleColor.DarkBlue; Console.Write "I"
            | Start -> Console.BackgroundColor <- ConsoleColor.DarkRed;  Console.Write "IS"
            | Finish -> Console.BackgroundColor <- ConsoleColor.DarkGreen; Console.Write "ISF"
    )

let private calcCostOfPath origin (path: SimpleWorld.Position list) =
    let rec c t (l: SimpleWorld.Position) (r: SimpleWorld.Position list) =
        match r with
        | [] -> t
        | _ -> c (t + l.costTo r.Head) r.Head r.Tail
    c 0.0 origin path

let createInstance() i width height thoroughnesses=
    su i Init
    let world = SimpleWorld.create width height
    let random = new Random()
    let rec chooseRandomNode() =
        match world.Positions.[random.Next(world.Positions.Count - 1)] with
        | p when not (p.Type = SimpleWorld.Wall) -> p
        | _ -> chooseRandomNode()
    {
        World = world
        Origin = chooseRandomNode()
        Destination = chooseRandomNode()
        Thoroughnesses = thoroughnesses
        Results = List.empty
    }

let createSolver origin destination thoroughness (world: SimpleWorld.World)=
    {
        Solver.create origin destination (world.neighbors) (fun f t -> f.costTo t) (fun f t -> f.distanceTo t) with
            Thoroughness = thoroughness
    }

let createSolvers instance =
    let cs t = createSolver instance.Origin instance.Destination t instance.World
    {
        instance with Results = List.map (cs) instance.Thoroughnesses
    }

let solveSolvers i instance =
    su i Start
    let t = {
        instance with Results = List.map (Solver.solve) instance.Results
    }
    su i Finish
    t

let resultToString i width height result =
    i.ToString() + "," +
    width.ToString() + "x" + height.ToString() + "," +
    (result.EstimateScore result.OriginNode result.DestinationNode).ToString() + "," +
    result.Thoroughness.ToString() + "," +
    result.Path.Length.ToString() + "," +
    (calcCostOfPath result.OriginNode result.Path).ToString() + "," +
    result.Ticks.ToString()

let getResults i width height instance =
    List.map (resultToString i width height) instance.Results

let parallelRun (list: 'T list) runner = ParallelEnumerable.AsParallel(list).ForAll(new Action<'T>(runner))

let fMon = new Object()
let writeResults file lines =
    lock fMon (fun () -> 
        File.AppendAllLines(file, lines)
    )

let rec run() =
    Console.Write("Number of tests? ")
    let numOfRounds = Console.ReadLine() |> int
    Console.Write("Width? ")
    let width = Console.ReadLine() |> int
    Console.Write("Height? ")
    let height = Console.ReadLine() |> int

    Console.Clear()

    let fileName = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + "/FAStarTests/Data.csv"

    if not (File.Exists fileName) then File.WriteAllLines(fileName, ["testNum,size,estimate,thoroughness,pathLength,pathCost,ticks"])
    
    let rounds = [1..numOfRounds]
    let thoroughnesses = List.init 101 (fun t -> 0.01 * (t |> float))

    let runner i = createInstance() i width height thoroughnesses |> createSolvers |> solveSolvers i |> getResults i width height |> writeResults fileName

//    for i in rounds do runner i
    parallelRun rounds (runner)

    Console.ResetColor()
    Console.Clear()

    Console.WriteLine("Continue? ([y]/n)")
    match Console.ReadKey().KeyChar with
        | 'n' -> 0
        | _ -> run()

[<EntryPoint>]
let main argv =
    run()

