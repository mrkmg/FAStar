namespace FAStar

open System
open System.Collections.Specialized
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
        member this.currentFromScore = this.FromScores.[this.CurrentNode]
        member this.neighbors = this.GetNeighbors this.CurrentNode
        member this.fromScore other = this.currentFromScore + this.CalcScore this.CurrentNode other
        member this.totalScore other = (this.fromScore other ) * this.Thoroughness + (this.EstimateScore other this.DestinationNode) * (1.0 - this.Thoroughness)
        member this.isSolved = Set.contains this.DestinationNode this.ClosedNodes
        member this.isUnsolveable = 0 = OrderedList.count this.OpenNodes
        member this.isValidNeighbor other =
            not (this.ClosedNodes.Contains(other)) &&
            (
                not (OrderedList.contains other this.OpenNodes) ||
                (this.fromScore other) < this.FromScores.[other]
            )
        member this.path =
            if not (this.isSolved) then []
            else
                let rec buildNodes cNode (arr: 'T list) =
                    if this.Parents.ContainsKey cNode then
                        buildNodes this.Parents.[cNode] (cNode :: arr)
                    else
                        arr
                buildNodes this.DestinationNode [] |> List.rev


[<RequireQualifiedAccess>]
module Solver =
    let create origin destination getNeigbors calcScore estimateScore =
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
                OpenNodes = solver.OpenNodes |> OrderedList.add (solver.totalScore neighbor) neighbor
                Parents = solver.Parents |> Map.add neighbor solver.CurrentNode
                FromScores = solver.FromScores |> Map.add neighbor (solver.fromScore neighbor)
        }

    let private processNeighbors (solver: Solver<'T>) =
         solver.neighbors |> List.where solver.isValidNeighbor |> List.fold processNeighbor solver

    let private checkStatus (solver: Solver<'T>) =
        match solver with
        | s when s.isSolved -> Solved
        | s when s.Ticks > s.MaxTicks -> TickLimitReached
        | s when s.isUnsolveable -> Unsolveable
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
