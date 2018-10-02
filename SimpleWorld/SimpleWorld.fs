namespace SimpleWorld

module SimpleWorld =
    open SharpNoise
    open System

    let private getXyFromIndex (index: int) (width: int) =
        (index % width, index / width)

    let private getIndexFromXy x y width =
        y * width + x

    type PositionType = Wall | Path | Grass | Rock

    type Position =
        {
            X: int
            Y: int
            Type: PositionType
        } with
        member this.distanceTo position =
            let sq num = num * num
            let x3 num = num * (double 3)
            sq (this.X - position.X) + sq (this.Y - position.Y) |> double |> sqrt |> x3

        member this.travelCost =
            match this.Type with
                | Path -> 1
                | Grass -> 2
                | Rock -> 3
                | _ -> Int32.MaxValue

        member this.costTo position =
            (this.distanceTo position) + (position.travelCost |> double)


    type World =
        {
            Width: int
            Height: int
            Positions: Position list
        } with
        member this.getAt x y = this.Positions.[getIndexFromXy x y this.Width]
        member this.neighbors position =
            [(position.X - 1, position.Y); (position.X + 1, position.Y); (position.X, position.Y - 1); (position.X, position.Y + 1)]
            |> List.where (fun (x, y) -> x >= 0 && x <= (this.Width - 1) && y >= 0 && y <= (this.Height - 1))
            |> List.map (fun (x, y) -> this.getAt x y)
            |> List.where (fun t -> not (t.Type = Wall))


    let private typeBound0 = -0.5 |> float32
    let private typeBound1 = 0 |> float32
    let private typeBound2 = 0.5 |> float32

    let private getTypeFromVal v =
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

    let private createNode index width (noise: NoiseMap) =
        let (x, y) = getXyFromIndex index width
        {
            X = x
            Y = y
            Type = (getTypeFromVal (noise.GetValue(x, y)))
        }

    let create width height =
        let noise = createSimpleNoiseMap width height
        {
            Width = width
            Height = height
            Positions = List.init (width * height) (fun i -> createNode i width noise)
        }


