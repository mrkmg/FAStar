namespace LargerInteractive

#nowarn "9"

open Eto.Drawing
open Eto.Forms
open SimpleWorld

type DrawType = Current | Path | EndPoint

type DrawWidget (width: int, height: int, scale: int) as this =
    inherit Drawable()

    let positionColor (pos: SimpleWorld.Position) =
        match pos.Type with
        | SimpleWorld.Teleporter -> Color.FromRgb(0xFFFF00)
        | SimpleWorld.Grass      -> Color.FromRgb(0x009900)
        | SimpleWorld.Path       -> Color.FromRgb(0xDDDDDD)
        | SimpleWorld.Rock       -> Color.FromRgb(0x555555)
        | _                      -> Color.FromRgb(0x000000)

    let typeColor pos t =
        match t with
        | Current -> Color.Blend(positionColor pos, Color.FromArgb(0x99FF0000))
        | Path -> Color.Blend(positionColor pos, Color.FromArgb(0x99FF0000))
        | EndPoint -> Color.FromRgb(0xFF0000)

    let sourceBitmap = new Bitmap(width * scale, height * scale, PixelFormat.Format32bppRgba)
    let drawHandle (g: Graphics) = g.DrawImage(sourceBitmap, float32 0.0, float32 0.0)

    do
        this.Width <- width * scale
        this.Height <- height * scale
        this.Paint.Add(fun e -> drawHandle e.Graphics)

    member this.setPixel (x: int) (y: int) (color: Color) (bitmapData: BitmapData) =
        let xMin = x * scale
        let yMin = y * scale
        let xMax = xMin + scale - 1
        let yMax = yMin + scale - 1
        for ix in [xMin .. xMax] do
            for iy in [yMin .. yMax] do
                bitmapData.SetPixel(ix, iy, color)

    member this.drawPosition (pos: SimpleWorld.Position) = 
        let bitmapData = sourceBitmap.Lock()
        this.setPixel pos.X pos.Y (positionColor pos) bitmapData
        bitmapData.Dispose()
        this.Invalidate()

    member this.drawPositionType (pos: SimpleWorld.Position) t =
        let bitmapData = sourceBitmap.Lock()
        this.setPixel pos.X pos.Y (typeColor pos t) bitmapData
        bitmapData.Dispose()
        this.Invalidate()

    member this.drawAll (posList: SimpleWorld.Position list) =
        let bitmapData = sourceBitmap.Lock()
        for pos in posList do this.setPixel pos.X pos.Y (positionColor pos) bitmapData
        bitmapData.Dispose()
        this.Invalidate()

    member this.drawAllType (posList: SimpleWorld.Position list) t = 
        let bitmapData = sourceBitmap.Lock()
        for pos in posList do this.setPixel pos.X pos.Y (typeColor pos t) bitmapData
        bitmapData.Dispose()
        this.Invalidate()
