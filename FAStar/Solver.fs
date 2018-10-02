namespace FAStar

module Solver =
    open System
    open Helpers

    exception NotPathable of string
    exception Unsolveable of string

    type private GetNeighbors<'Node> = 'Node -> 'Node list
    type private CalcScore<'Node> = 'Node -> 'Node -> double
    type private EstimateScore<'Node> = 'Node -> 'Node -> double

    type State<'Node when 'Node : comparison> =
        {
            OpenNodes: Set<'Node>
            ClosedNodes: Set<'Node>
            Parents: Map<'Node, 'Node>
            FromScores: Map<'Node, double>
            TotalScores: Map<'Node, double>
            OriginNode: 'Node
            DestinationNode: 'Node
            LastNode: 'Node
            GetNeighbors: GetNeighbors<'Node>
            CalcScore: CalcScore<'Node>
            EstimateScore: EstimateScore<'Node>
            NumberOfTicks: int
            Thoroughness: float
            MaxTicks: int
            Iter: State<'Node> -> unit
        } with
            member this.isSolved = Set.contains this.DestinationNode this.ClosedNodes
            member this.isUnsolveable = 0 = Set.count this.OpenNodes
            member this.path =
                if not (this.isSolved) then raise (NotPathable("Not Solved"))
                let rec buildNodes cNode (arr: List<'Node>) =
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
            NumberOfTicks = 0
            Thoroughness = 0.5
            MaxTicks = Int32.MaxValue
            Iter = fun t -> ()
        }

    let tick state =
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
                NumberOfTicks = state.NumberOfTicks + 1
        }
        newState.Iter newState
        newState

    let rec solve (state: State<'Node>) =
        if state.isSolved then state
        else if state.isUnsolveable then raise (Unsolveable("No Possible Path"))
        else if state.NumberOfTicks >= state.MaxTicks then raise (Unsolveable("Tick Limit Reached"))
        else state |> tick |> solve
