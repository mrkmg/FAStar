namespace LargerInteractive

open System
open FAStar
open SimpleWorld


type StateStatus = New | Ticking | Solved | Waiting | Done | Failed
type State =
    {
        Thoroughness: float
        Origin: SimpleWorld.Position
        Destination: SimpleWorld.Position
        World: SimpleWorld.World
        Solver: Solver<SimpleWorld.Position>
        Status: StateStatus
    }


module State =
    let random = new Random()

    let rec getRandomNode() (world: SimpleWorld.World) =
        match world.Positions.[random.Next(world.Positions.Count - 1)] with
            | p when not (p.Type = SimpleWorld.Wall) -> p
            | _ -> getRandomNode() world

    let makeSolver() origin destination (world: SimpleWorld.World) thoroughness =
        { Solver.create origin destination (world.neighbors) (fun t -> t.costTo) (fun t -> t.distanceTo) with
            Thoroughness = thoroughness }

    let create width height thoroughness =
        let world = SimpleWorld.create width height
        let origin = getRandomNode() world
        let destination = getRandomNode() world
        {
            Thoroughness = thoroughness
            Origin = origin
            Destination = destination
            World = world
            Solver = makeSolver() origin destination world thoroughness
            Status = New
        }

    let changeThoroughness thoroughness state =
        {
            state with
                Thoroughness = thoroughness
                Solver = makeSolver() state.Origin state.Destination state.World thoroughness
                Status = New
        }
        
    let reset state =
        {
            state with
                Solver = makeSolver() state.Origin state.Destination state.World state.Thoroughness
                Status = New
        }

    let isNodeCurrentable state node = not (state.Origin = node || state.Destination = node)
    let printUndoClosed (drawWidget: DrawWidget) state = drawWidget.drawAll (Set.toList state.Solver.ClosedNodes)
    let printMap (drawWidget: DrawWidget) state = drawWidget.drawAll (Map.toList state.World.Positions |> List.map (fun (k,v) -> v))
    let printEndPoints (drawWidget: DrawWidget) state =
        drawWidget.drawPositionType state.Origin EndPoint
        drawWidget.drawPositionType state.Destination EndPoint
    let printCurrent (drawWidget: DrawWidget) state =
        let notIsOrigin = not (state.Solver.CurrentNode = state.Origin)
        let notIsDestination =  not (state.Solver.CurrentNode = state.Destination)
        if notIsOrigin && notIsDestination then drawWidget.drawPositionType state.Solver.CurrentNode Current
    let printManyCurrents (drawWidget: DrawWidget) currents =
        drawWidget.drawAllType currents Current
    let printPath (drawWidget: DrawWidget) state =
        if state.Solver.Status = SolverStatus.Solved then drawWidget.drawAllType state.Solver.Path Path

    let tickState (drawWidget: DrawWidget) allowedMilliseconds state =
        match state.Status with
            | StateStatus.New ->
                printMap drawWidget state
                printEndPoints drawWidget state
                { state with Status = StateStatus.Waiting }
            | StateStatus.Ticking ->
                let stopTime = DateTime.Now.AddMilliseconds(allowedMilliseconds)
                let mutable tmpState = state
                let mutable tmpNodes = []
                while DateTime.Now < stopTime && tmpState.Solver.Status = Open do
                    tmpState <- { tmpState with Solver = Solver.tick tmpState.Solver }
                    if isNodeCurrentable tmpState tmpState.Solver.CurrentNode then
                        tmpNodes <- tmpState.Solver.CurrentNode :: tmpNodes
                printManyCurrents drawWidget tmpNodes
                match tmpState.Solver.Status with
                    | SolverStatus.Open -> tmpState
                    | SolverStatus.Solved -> { tmpState with Status = StateStatus.Solved }
                    | _ -> { tmpState with Status = StateStatus.Failed }
            | StateStatus.Solved ->
                printUndoClosed drawWidget state
                printEndPoints drawWidget state
                printPath drawWidget state
                { state with Status = StateStatus.Waiting }
            | _ -> state
