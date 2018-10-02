namespace FAStar

module Solver =
    open System
    open Helpers

    exception NotPathable
    exception MaxTickReached
    exception Unsolveable
    exception AlreadySolved

    type private GetNeighbors<'T> = 'T -> 'T list
    type private CalcScore<'T> = 'T -> 'T -> double
    type private EstimateScore<'T> = 'T -> 'T -> double

    type State<'T when 'T : comparison> =
        {
            OpenNodes: Set<'T>
            ClosedNodes: Set<'T>
            Parents: Map<'T, 'T>
            FromScores: Map<'T, double>
            TotalScores: Map<'T, double>
            OriginNode: 'T
            DestinationNode: 'T
            LastNode: 'T
            GetNeighbors: GetNeighbors<'T>
            CalcScore: CalcScore<'T>
            EstimateScore: EstimateScore<'T>
            Ticks: int
            Thoroughness: float
            MaxTicks: int
            Iter: State<'T> -> unit
        } with
            member this.isSolved = Set.contains this.DestinationNode this.ClosedNodes
            member this.isUnsolveable = 0 = Set.count this.OpenNodes
            member this.path =
                if not (this.isSolved) then raise (NotPathable)
                let rec buildNodes cNode (arr: List<'T>) =
                    if this.Parents.ContainsKey cNode then
                        buildNodes this.Parents.[cNode] (cNode :: arr)
                    else
                        arr
                buildNodes this.DestinationNode [] |> List.rev

    let create origin destination getNeigbors calcScore estimateScore =
        {
            OpenNodes = Set.empty |> Set.add origin
            ClosedNodes = Set.empty
            Parents = Map.empty
            OriginNode = origin
            DestinationNode = destination
            FromScores = Map.empty |> Map.add origin (double 0)
            TotalScores = Map.empty |> Map.add origin (double 0)
            LastNode = origin
            GetNeighbors = getNeigbors
            CalcScore = calcScore
            EstimateScore = estimateScore
            Ticks = 0
            Thoroughness = 0.5
            MaxTicks = Int32.MaxValue
            Iter = fun t -> ()
        }

    let tick (state: State<'T>) =
        if state.isSolved then raise (AlreadySolved)
        if state.isUnsolveable then raise (Unsolveable)
        else if state.Ticks >= state.MaxTicks then raise (MaxTickReached)

        let sortByer t = state.TotalScores.[t]
        let current = state.OpenNodes |> Set.toSeq |> Seq.sortBy sortByer |> Seq.head
        let currentFromScore = state.FromScores.[current]
        let calcFromScore node = currentFromScore + (state.CalcScore current node)
        let calcTotalScore node = (calcFromScore node) * state.Thoroughness + (state.EstimateScore node state.DestinationNode) * (1.0 - state.Thoroughness)

        let filterNeighbor node =
            not (Set.contains node state.ClosedNodes) &&
            (
                not (Set.contains node state.OpenNodes) ||
                (calcFromScore node) < state.FromScores.[node]
            )

        let newNodes = state.GetNeighbors current |> List.where filterNeighbor
        let newState = {
            state with
                LastNode = current
                OpenNodes = appendListToSet newNodes (state.OpenNodes.Remove current)
                ClosedNodes = state.ClosedNodes.Add current
                Parents = transformAppendListToMap newNodes (fun t -> current) state.Parents
                FromScores = transformAppendListToMap newNodes (calcFromScore) state.FromScores
                TotalScores = transformAppendListToMap newNodes (calcTotalScore) state.TotalScores
                Ticks = state.Ticks + 1
        }
        newState.Iter newState
        newState

    let rec solve (state: State<'T>) =
        if state.isSolved then state
        else state |> tick |> solve
