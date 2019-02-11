namespace LargerInteractive

open Eto.Forms

type MainForm () as this =
    inherit Form()
    do
        let worldWidth = 1600
        let worldHeight = 800
        let scale = 1
        
        let fps = 20.0
        let millisecondsNeedForRender = 5.0
        let millisecondsPerTick = 1000.0 / fps - millisecondsNeedForRender
        let secondsPerTick = 1.0 / fps

        base.Title <- "FAStar Interactive Test"

        let drawing = new DrawWidget(worldWidth, worldHeight, scale)

        let thoroughnessInput = new Slider(Value = 50, MinValue = 0, MaxValue = 100, Width = 200, TickFrequency = 5)

        let goButton = new Button(Text = "Go")
        let newMapButton = new Button(Text = "New Map")
        let quitButton = new Button(Text = "Quit")

        let barLayout = new StackLayout(Orientation = Orientation.Horizontal)
        barLayout.Items.Add(new StackLayoutItem(thoroughnessInput))
        barLayout.Items.Add(new StackLayoutItem(goButton))
        barLayout.Items.Add(new StackLayoutItem(newMapButton))
        barLayout.Items.Add(new StackLayoutItem(quitButton))

        let mainLayout = new StackLayout()
        mainLayout.Items.Add(new StackLayoutItem(drawing))
        mainLayout.Items.Add(new StackLayoutItem(barLayout))
        
        this.Content <- mainLayout

        let mutable (mainState: State option) = None

        let getCurrentThoroughness() = (float thoroughnessInput.Value) / (float 100);
        
        let tickState() = 
            match mainState with 
                | Some state -> mainState <- Some (State.tickState drawing millisecondsPerTick state)
                | None -> ()

        let createNewState() = mainState <- Some (State.create worldWidth worldHeight (getCurrentThoroughness()))

        let startSolve() = 
            match mainState with 
                | Some state -> mainState <- Some { (State.reset state) with Status = Ticking }
                | None -> ()
        
        let changeStateThoroughness() =  
            match mainState with
                | Some state -> mainState <- Some (State.changeThoroughness (getCurrentThoroughness()) state)
                | None -> ()
                
        let t = new UITimer()
        t.Interval <- secondsPerTick
        t.Elapsed.Add(fun e -> do tickState())
        
        goButton.Click.Add(fun e -> do startSolve())
        newMapButton.Click.Add(fun e -> do createNewState())
        quitButton.Click.Add(fun e -> t.Stop(); t.Dispose(); this.Close())
        thoroughnessInput.ValueChanged.Add(fun r -> do changeStateThoroughness())
        
        createNewState()
        t.Start()
