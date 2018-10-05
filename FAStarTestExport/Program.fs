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

let private calcCostOfPath origin (path: SimpleWorld.Position list) =
    let rec c t (l: SimpleWorld.Position) (r: SimpleWorld.Position list) =
        match r with
        | [] -> t
        | _ -> c (t + l.costTo r.Head) r.Head r.Tail
    c 0.0 origin path

let createInstance() width height thoroughnesses=
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
    do Console.Write("s" + i.ToString() + " ")
    let t = {
        instance with Results = List.map (Solver.solve) instance.Results
    }
    do Console.Write("e" + i.ToString() + " ")
    t

let resultToString i width height result =
    i.ToString() + "," +
    width.ToString() + "x" + height.ToString() + "," +
    (result.EstimateScore result.OriginNode result.DestinationNode).ToString() + "," +
    result.Thoroughness.ToString() + "," +
    result.Path.Length.ToString() + "," +
    (calcCostOfPath result.OriginNode result.Path).ToString() + "," +
    result.Ticks.ToString()

let writeResultsToFile i width height (fileWriter: StreamWriter) instance =
    for r in instance.Results do
        fileWriter.WriteLine(resultToString i width height r)

let parallelRun (list: 'T list) runner = ParallelEnumerable.AsParallel(list).ForAll(new Action<'T>(runner))

[<EntryPoint>]
let main argv =
    Console.Write("Number of tests? ")
    let numOfRounds = Console.ReadLine() |> int
    Console.Write("Width? ")
    let width = Console.ReadLine() |> int
    Console.Write("Height? ")
    let height = Console.ReadLine() |> int

    if File.Exists("./test.csv") then File.Delete("./test.csv")

    let fileWrite = File.OpenWrite("./test.csv") |> (fun t -> new StreamWriter(t))
    let rounds = [1..numOfRounds]
    let thoroughnesses = List.init 20 (fun t -> 0.05 * (t |> float))

    do fileWrite.WriteLine("testNum,size,estimate,thoroughness,pathLength,pathCost,ticks")

    let runner i =
        createInstance() width height thoroughnesses |> createSolvers |> solveSolvers i |> writeResultsToFile i width height fileWrite
        do fileWrite.Flush()

//    for i in rounds do runner i
    parallelRun rounds (runner)

    fileWrite.Close()
    fileWrite.Dispose()
    0

