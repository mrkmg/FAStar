namespace LargerInteractive

open FAStar
open SimpleWorld
open System
open Eto.Forms
open Eto.Drawing

type MainForm () as this =
    inherit Form()
    do
        let worldWidth = 200
        let worldHeight = 100
        let scale = 8

        base.Title <- "FAStar Interactive Test"

        let drawing = new DrawWidget(worldWidth, worldHeight, scale)

        let thoroughnessInput = new Slider()
        thoroughnessInput.Value <- 50
        thoroughnessInput.MinValue <- 0
        thoroughnessInput.MaxValue <- 100
        thoroughnessInput.Width <- 200
        thoroughnessInput.TickFrequency <- 5

        let goButton = new Button()
        goButton.Text <- "Go"

        let newMapButton = new Button()
        newMapButton.Text <- "New Map"

        let quitButton = new Button()
        quitButton.Text <- "Quit"

        let mainLayout = new StackLayout()
        mainLayout.Items.Add(new StackLayoutItem(drawing))

        let barLayout = new StackLayout()
        barLayout.Orientation <- Orientation.Horizontal
        barLayout.Items.Add(new StackLayoutItem(thoroughnessInput))
        barLayout.Items.Add(new StackLayoutItem(goButton))
        barLayout.Items.Add(new StackLayoutItem(newMapButton))
        barLayout.Items.Add(new StackLayoutItem(quitButton))

        mainLayout.Items.Add(new StackLayoutItem(barLayout))

        this.Content <- mainLayout

        let mutable (mainState: State option) = None

        let getCurrentThoroughness() = (float thoroughnessInput.Value) / (float 100);

        let tickState() = 
            match mainState with 
                | Some state -> mainState <- Some (State.tickState drawing state)
                | None -> ()

        let createNewState() = mainState <- Some (State.create worldWidth worldHeight (getCurrentThoroughness()))

        let startSolve() = 
            match mainState with 
                | Some state -> mainState <- Some { state with Status = Ticking }
                | None -> ()
        
        let changeStateThoroughness() =  
            match mainState with
                | Some state -> mainState <- Some (State.changeThoroughness (getCurrentThoroughness()) state)
                | None -> ()

        let t = new UITimer()
        t.Interval <- 0.0001
        t.Elapsed.Add(fun e -> do tickState())
        goButton.Click.Add(fun e -> do startSolve())
        newMapButton.Click.Add(fun e -> do createNewState())
        quitButton.Click.Add(fun e -> t.Stop(); t.Dispose(); this.Close())
        thoroughnessInput.MouseUp.Add(fun r -> do changeStateThoroughness())
        t.Start()
