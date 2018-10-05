namespace SimpleWorld

module SimpleWorld =
    open SharpNoise
    open System

    let private teleportFactor = 400
    let private factor = 16.0
    let private cFactor i = factor ** i
    let private sq (i: float) = i * i
    let private dist x1 x2 y1 y2 =
        sq (float x1 - float x2) + sq (float y1 - float y2) |> sqrt

    let private getXyFromIndex (index: int) (width: int) =
        (index % width, index / width)

    let private getIndexFromXy x y width =
        y * width + x

    type PositionType = Wall | Path | Grass | Rock | Teleporter

    type Position =
        {
            X: int
            Y: int
            Type: PositionType
            TeleportPosition: int option
        } with
        member this.travelCost =
            match this.Type with
                | Teleporter -> 0.0
                | Path -> cFactor 0.0
                | Grass -> cFactor 1.0
                | Rock -> cFactor 2.0
                | Wall -> 0.0
        member this.distanceTo position = (dist this.X position.X this.Y position.Y) * (cFactor 1.0)
        member this.costTo position =
            match this.Type with
            | Teleporter -> 0.0
            | _ -> (this.distanceTo position) + (position.travelCost |> float)


    type World =
        {
            Width: int
            Height: int
            Positions: Map<int, Position>
        } with
        member this.getAt x y = this.Positions.[getIndexFromXy x y this.Width]
        member this.neighbors position =
            match position.Type with
                | Teleporter -> [getXyFromIndex position.TeleportPosition.Value this.Width]
                | _ -> [
                           (position.X - 1, position.Y)
                           (position.X + 1, position.Y)
                           (position.X, position.Y - 1)
                           (position.X, position.Y + 1)
                           (position.X - 1, position.Y - 1)
                           (position.X - 1, position.Y + 1)
                           (position.X + 1, position.Y - 1)
                           (position.X + 1, position.Y + 1)
                       ]
            |> List.where (fun (x, y) -> x >= 0 && x <= (this.Width - 1) && y >= 0 && y <= (this.Height - 1))
            |> List.map (fun (x, y) -> this.getAt x y)
            |> List.where (fun t -> not (t.Type = Wall))


    let private typeBound0 = -0.5 |> float32
    let private typeBound1 = 0 |> float32
    let private typeBound2 = 0.5 |> float32

    let private getTypeFromVal v =
        if (new Random()).Next(teleportFactor) = 1 then Teleporter
        else
            match v with
                | t when t < typeBound0 -> Wall
                | t when t < typeBound1 && t > typeBound0 -> Grass
                | t when t < typeBound2 && t > typeBound1 -> Path
                | _ -> Rock

    let private createSimpleNoiseMap width height =
        let map = new NoiseMap()
        let source = new SharpNoise.Modules.Perlin()
        source.Seed <- (new Random()).Next()
        let builder = SharpNoise.Builders.PlaneNoiseMapBuilder()
        builder.DestNoiseMap <- map
        builder.SourceModule <- source
        builder.SetDestSize(width, height)
        builder.SetBounds(float -3, float 3, float -2, float 2)
        builder.Build()
        map

    let private createNode index width height (noise: NoiseMap) =
        let (x, y) = getXyFromIndex index width
        let t = (getTypeFromVal (noise.GetValue(x, y)))
        {
            X = x
            Y = y
            Type = t
            TeleportPosition = if t = Teleporter then Some ((new Random()).Next(width * height)) else None
        }

    let create width height =
        let noise = createSimpleNoiseMap width height
        {
            Width = width
            Height = height
            Positions = List.fold (fun m i -> Map.add i (createNode i width height noise) m) Map.empty [0..width * height]
        }


