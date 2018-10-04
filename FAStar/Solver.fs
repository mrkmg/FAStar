namespace FAStar

open System
open Helpers

type SolverStatus = Open | Solved | Unsolveable | TickLimitReached

type private GetNeighbors<'T> = 'T -> 'T list
type private CalcScore<'T> = 'T -> 'T -> double
type private EstimateScore<'T> = 'T -> 'T -> double

type Solver<'T when 'T : comparison> =
    {
        OpenNodes: Set<'T>
        ClosedNodes: Set<'T>
        Parents: Map<'T, 'T>
        FromScores: Map<'T, double>
        TotalScores: Map<'T, double>
        OriginNode: 'T
        DestinationNode: 'T
        CurrentNode: 'T
        GetNeighbors: GetNeighbors<'T>
        CalcScore: CalcScore<'T>
        EstimateScore: EstimateScore<'T>
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
        member this.isUnsolveable = 0 = Set.count this.OpenNodes
        member this.isValidNeighbor other = not (this.ClosedNodes.Contains(other)) && ( not (this.OpenNodes.Contains(other)) || (this.fromScore other) < this.FromScores.[other])
        member this.path =
            if not (this.isSolved) then []
            else
                let rec buildNodes cNode (arr: 'T list) =
                    if this.Parents.ContainsKey cNode then
                        buildNodes this.Parents.[cNode] (cNode :: arr)
                    else
                        arr
                buildNodes this.DestinationNode [] |> List.rev


module Solver =
    let create origin destination getNeigbors calcScore estimateScore =
        {
            OpenNodes = Set.empty |> Set.add origin
            ClosedNodes = Set.empty
            Parents = Map.empty
            OriginNode = origin
            DestinationNode = destination
            FromScores = Map.empty |> Map.add origin (double 0)
            TotalScores = Map.empty |> Map.add origin (double 0)
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

    let private setCurrentNode (state: Solver<'T>) =
        let next = state.OpenNodes |> Set.toSeq |> Seq.sortBy (fun t -> state.TotalScores.[t]) |> Seq.head // TODO - Make better
        {
            state with
                CurrentNode = next
                OpenNodes = state.OpenNodes |> Set.remove next
                ClosedNodes = state.ClosedNodes |> Set.add next
        }

    let private processNeighbor (state: Solver<'T>) neighbor =
        { 
            state with 
                OpenNodes = state.OpenNodes.Add neighbor
                Parents = state.Parents |> Map.add neighbor state.CurrentNode
                FromScores = state.FromScores |> Map.add neighbor (state.fromScore neighbor)
                TotalScores = state.TotalScores |> Map.add neighbor (state.totalScore neighbor)
        }

    let private processNeighbors (state: Solver<'T>) =
         state.neighbors |> List.where state.isValidNeighbor |> List.fold processNeighbor state 

    let private checkStatus (state: Solver<'T>) =
        match state with
        | s when s.isSolved -> Solved
        | s when s.Ticks > s.MaxTicks -> TickLimitReached
        | s when s.isUnsolveable -> Unsolveable
        | _ -> state.Status

    let private postProcess (state: Solver<'T>) =
        {
            state with
                Ticks = state.Ticks + 1
                Status = checkStatus state
        } |> tap state.Iter

    let tick (state: Solver<'T>) =
        if not (state.Status = Open) then state
        else state |> setCurrentNode |> processNeighbors |> postProcess

    let rec solve (state: Solver<'T>) =
        match tick state with
            | nextState when nextState.Status = Open -> solve nextState
            | nextState -> nextState 
