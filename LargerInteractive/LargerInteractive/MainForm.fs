namespace LargerInteractive

open System
open Eto.Forms
open Eto.Drawing

type Drawthing () as this =
    inherit Drawable()
    do
        let drawHandle (g: Graphics) (p: Pen) (len: single) (a: single)



type MainForm () as this =
    inherit Form()
    do
        base.Title <- "My Eto Form"
        base.ClientSize <- new Size(400, 400)

        // table with three rows
        let layout = new StackLayout()
        layout.Items.Add(new StackLayoutItem(new Label(Text = "Hello World!")))
        // Add more controls here

        let drawing = new base 

        base.Content <- layout;
