namespace FAStar

open System
open Helpers

type SolverStatus = Open | Solved | Unsolveable | TickLimitReached

type Solver<'T when 'T : comparison> =
    {
        OpenNodes: OrderedList<float, 'T>
        ClosedNodes: Set<'T>
        Parents: Map<'T, 'T>
        FromScores: Map<'T, float>
        OriginNode: 'T
        DestinationNode: 'T
        CurrentNode: 'T
        GetNeighbors: 'T -> 'T list
        CalcScore: 'T -> 'T -> float
        EstimateScore: 'T -> 'T -> float
        Ticks: int
        Thoroughness: float
        MaxTicks: int
        Status: SolverStatus
        Iter: Solver<'T> -> unit
    } with
        member this.Path =
            if not (this.Status = Solved) then []
            else
                let rec buildNodes cNode (arr: 'T list) =
                    if this.Parents.ContainsKey cNode then
                        buildNodes this.Parents.[cNode] (cNode :: arr)
                    else
                        arr
                buildNodes this.DestinationNode []


[<RequireQualifiedAccess>]
module Solver =
    let create (origin: 'T when 'T : comparison) destination getNeigbors calcScore estimateScore =
        {
            OpenNodes = OrderedList.empty |> OrderedList.add 0.0 origin
            ClosedNodes = Set.empty
            Parents = Map.empty
            OriginNode = origin
            DestinationNode = destination
            FromScores = Map.empty |> Map.add origin 0.0
            CurrentNode = origin
            GetNeighbors = getNeigbors
            CalcScore = calcScore
            EstimateScore = estimateScore
            Ticks = 0
            Thoroughness = 0.5
            MaxTicks = Int32.MaxValue
            Iter = fun (t: Solver<'T>) -> ()
            Status = Open
        }

    let private currentFromScore solver = solver.FromScores.[solver.CurrentNode]
    let private neighbors solver = solver.GetNeighbors solver.CurrentNode
    let private fromScore solver other = (currentFromScore solver) + (solver.CalcScore solver.CurrentNode other)
    let private totalScore solver other = (fromScore solver other) * solver.Thoroughness + (solver.EstimateScore other solver.DestinationNode) * (1.0 - solver.Thoroughness)
    let private isSolved solver = solver.ClosedNodes.Contains(solver.DestinationNode)
    let private isUnsolveable solver = OrderedList.count solver.OpenNodes = 0
    let private isValidNeighbor solver other =
        not (solver.ClosedNodes.Contains(other)) &&
        (
            not (OrderedList.contains other solver.OpenNodes) ||
            (fromScore solver other) < solver.FromScores.[other]
        )

    let private setCurrentNode (solver: Solver<'T>) =
        let (c, n) = solver.OpenNodes |> OrderedList.pop
        {
            solver with
                CurrentNode = c
                OpenNodes = n
                ClosedNodes = solver.ClosedNodes |> Set.add c
        }

    let private processNeighbor (solver: Solver<'T>) neighbor =
        { 
            solver with
                OpenNodes = solver.OpenNodes |> OrderedList.add (totalScore solver neighbor) neighbor
                Parents = solver.Parents |> Map.add neighbor solver.CurrentNode
                FromScores = solver.FromScores |> Map.add neighbor (fromScore solver neighbor)
        }

    let private processNeighbors (solver: Solver<'T>) =
         neighbors solver |> List.where (isValidNeighbor solver) |> List.fold processNeighbor solver

    let private checkStatus (solver: Solver<'T>) =
        match solver with
            | s when isSolved s -> Solved
            | s when s.Ticks > s.MaxTicks -> TickLimitReached
            | s when isUnsolveable s -> Unsolveable
            | _ -> solver.Status

    let private postProcess (solver: Solver<'T>) =
        {
            solver with
                Ticks = solver.Ticks + 1
                Status = checkStatus solver
        } |> tap solver.Iter

    let tick (solver: Solver<'T>) =
        if not (solver.Status = Open) then solver
        else solver |> setCurrentNode |> processNeighbors |> postProcess

    let rec solve (solver: Solver<'T>) =
        match tick solver with
            | nextState when nextState.Status = Open -> solve nextState
            | nextState -> nextState
