namespace FAStarInteractiveTest

module Display =
    open System.Drawing
    open System
    open SimpleWorld.SimpleWorld

    let fillerChar = '▓'

    let drawAt() (x: int) (y: int) (c: char) =
        Console.CursorLeft <- x
        Console.CursorTop <- y
        Console.Write c

    let drawMessage() (message: string) =
        Console.CursorLeft <- 0
        Console.CursorTop <- Console.WindowHeight - 2
        Console.Write message

    let clearScreen() =
        Console.BackgroundColor <- ConsoleColor.Black
        Console.Clear()

    let getCharForNode (node: Position) =
        match node.Type with
        | PositionType.Wall -> 'W'
        | PositionType.Path -> 'P'
        | PositionType.Grass -> 'G'
        | PositionType.Rock -> 'R'

    let getColorForNode (node: Position) =
        match node.Type with
        | PositionType.Wall -> ConsoleColor.Black
        | PositionType.Path -> ConsoleColor.White
        | PositionType.Rock -> ConsoleColor.DarkGray
        | PositionType.Grass -> ConsoleColor.Green

    let promptToContinue() =
        Console.BackgroundColor <- ConsoleColor.Black
        drawMessage() "q to quit, any other key to continue"
        not (Console.ReadKey(false).KeyChar.Equals 'q')

    let askForSingleKey() =
        Console.ReadKey(false)

    let askForSingleKeyWithMessage() msg =
        Console.BackgroundColor <- ConsoleColor.Black
        Console.ForegroundColor <- ConsoleColor.White
        drawMessage() msg
        askForSingleKey()

    let displayError() (msg: string) =
        Console.BackgroundColor <- ConsoleColor.Red
        Console.ForegroundColor <- ConsoleColor.White
        drawMessage() msg

    let printMessage() (message: string) =
        Console.BackgroundColor <- ConsoleColor.Black
        Console.ForegroundColor <- ConsoleColor.White
        drawMessage() (String.replicate Console.WindowWidth " ")
        drawMessage() message

    let printNode() node =
        Console.BackgroundColor <- getColorForNode node
        drawAt() node.X node.Y ' '

    let printNodes() (nodes: List<Position>) =
        clearScreen()
        for node in nodes do printNode() node

    let printPath() (nodes: List<Position>) =
        Console.ForegroundColor <- ConsoleColor.Blue
        for node in nodes do
            Console.BackgroundColor <- getColorForNode node
            drawAt() node.X node.Y fillerChar
        Console.ForegroundColor <- ConsoleColor.White

    let debugClosedNodes() (nodes: List<Position>) =
        Console.ForegroundColor <- ConsoleColor.Red
        for node in nodes do
            Console.BackgroundColor <- getColorForNode node
            drawAt() node.X node.Y fillerChar
        Console.ForegroundColor <- ConsoleColor.White

    let debugOpenNodes() (nodes: List<Position>) =
        Console.ForegroundColor <- ConsoleColor.Cyan
        for node in nodes do
            Console.BackgroundColor <- getColorForNode node
            drawAt() node.X node.Y fillerChar
        Console.ForegroundColor <- ConsoleColor.White

    let debugCurrentNode() (node: Position) =
        Console.BackgroundColor <- getColorForNode node
        Console.ForegroundColor <- ConsoleColor.Red
        drawAt() node.X node.Y fillerChar
        Console.ForegroundColor <- ConsoleColor.White

    let getNumberFromInput (message: string) =
        Console.Clear()
        Console.WriteLine message
        let mutable didGetNumber = false
        let mutable number = 0
        while not didGetNumber do
            let input = Console.ReadLine()
            try
                number <- input |> int
                if number > 500 then
                    Console.WriteLine "Number too large"
                else
                    didGetNumber <- number < 500
            with
            | :? FormatException -> displayError() "Invalid Number"
        number




