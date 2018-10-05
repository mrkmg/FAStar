namespace FAStarInteractiveTest

module Display =
    open System.Threading
    open System.Drawing
    open System
    open SimpleWorld.SimpleWorld

    let private fillerChar = '▓'
    let private defaultBackground = ConsoleColor.Black
    let private defaultForeground = ConsoleColor.White
    let private errorBackground = ConsoleColor.Red
    let private errorForeground = ConsoleColor.White
    let private pathForeground = ConsoleColor.Red
    let private endpointForground = ConsoleColor.Blue
    let private debugPointForeground = ConsoleColor.Red

    let drawAt() (x: int) (y: int) (c: char) =
        Console.CursorLeft <- x
        Console.CursorTop <- y
        Console.Write c

    let drawMessage() (message: string) =
        Console.CursorLeft <- 0
        Console.CursorTop <- Console.WindowHeight - 2
        Console.Write message

    let clearScreen() =
        Console.BackgroundColor <- defaultBackground
        Console.ForegroundColor <- defaultForeground
        Console.Clear()

    let getCharForNode (node: Position) =
        match node.Type with
        | PositionType.Wall -> 'W'
        | PositionType.Path -> 'P'
        | PositionType.Grass -> 'G'
        | PositionType.Rock -> 'R'
        | PositionType.Teleporter -> 'T'

    let getColorForNode (node: Position) =
        match node.Type with
        | PositionType.Wall -> ConsoleColor.Black
        | PositionType.Path -> ConsoleColor.White
        | PositionType.Rock -> ConsoleColor.DarkGray
        | PositionType.Grass -> ConsoleColor.Green
        | PositionType.Teleporter -> ConsoleColor.DarkYellow

    let promptToContinue() =
        Console.BackgroundColor <- defaultBackground
        Console.ForegroundColor <- defaultForeground
        drawMessage() "q to quit, any other key to continue"
        not (Console.ReadKey(true).KeyChar.Equals 'q')

    let askForSingleKey() =
        Console.ReadKey(true)

    let askForSingleKeyWithMessage() msg =
        Console.BackgroundColor <- defaultBackground
        Console.ForegroundColor <- defaultForeground
        drawMessage() msg
        askForSingleKey()

    let displayError() (msg: string) =
        Console.BackgroundColor <- errorBackground
        Console.ForegroundColor <- errorForeground
        Console.CursorLeft <- 0
        Console.CursorTop <- Console.WindowHeight - 1
        Console.Write msg

    let printMessage() (message: string) =
        Console.BackgroundColor <- defaultBackground
        Console.ForegroundColor <- defaultForeground
        drawMessage() (String.replicate 20 " ")
        drawMessage() message

    let printNode() node =
        Console.BackgroundColor <- getColorForNode node
        drawAt() node.X node.Y ' '

    let printNodes() (nodes: List<Position>) =
        clearScreen()
        for node in nodes do printNode() node

    let printPath() (nodes: List<Position>) =
        Console.ForegroundColor <- pathForeground
        for node in nodes do
            Console.BackgroundColor <- getColorForNode node
            drawAt() node.X node.Y fillerChar
            Thread.Sleep 20

    let debugCurrentNode() (node: Position) =
        Console.BackgroundColor <- getColorForNode node
        Console.ForegroundColor <- debugPointForeground
        drawAt() node.X node.Y fillerChar

    let debugEndPointNode() (node: Position) =
        Console.BackgroundColor <- getColorForNode node
        Console.ForegroundColor <- endpointForground
        drawAt() node.X node.Y fillerChar




