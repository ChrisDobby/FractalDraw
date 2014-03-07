namespace FractalDraw

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

[<Measure>]
type radians

[<Measure>]
type pi

[<JavaScript>]
module Fractals =

    type Colour =
        | Random
        | Defined of red : string * green : string * blue : string

    type TypeOfShape = 
        | Line of width : string * length : string
        | Circle of radius : string
        | Rectangle of width : string * height : string

    type Drawing =
        {
            MaxIterations : int
            Angle : float<radians/pi>
            X : float
            Y : float
            Shapes : list<Shape>
            Branches : list<Branch>
        }
    and Branch = 
        {
            AngleChange : float<radians/pi>
        }
    and Shape =
        {
            Type : TypeOfShape
            Colour : Colour
        }

    let formHeight = 550.             

    let private PI = 3.14159265359<pi>

    let private endpoint x y (angle:float<radians/pi>) length =
        let angleRadians = float (PI * angle)
        x + length * cos angleRadians,
        y + length * sin angleRadians
    
    let private flip y = (float)formHeight - y    
    
     // Since IE does not support canvas natively. Initialization of the 
    // canvas element is done through the excanvas.js library.
    [<Inline "G_vmlCanvasManager.initElement($elem)">]
    let private initialize (elem: CanvasElement) : unit = ()
        
    let private rand max = EcmaScript.Math.Floor(EcmaScript.Math.Random() * float(max + 1))

    let private randomColour() = rand 255, rand 255, rand 255

    let private drawRectangle (ctx:CanvasRenderingContext2D) (x : float) (y : float) (width : float) (height : float) (colour:int * int * int) =
        let r, g, b = colour        
        ctx.FillStyle <- "rgb(" + r.ToString() + "," + g.ToString() + "," + b.ToString() + ")"
        ctx.FillRect(x, y |> flip, width, height)

    let private drawCircle (ctx:CanvasRenderingContext2D) (x : float) (y : float) (radius : int) (colour:int * int * int) =
        let r, g, b = colour
        ctx.StrokeStyle <- "rgb(" + r.ToString() + "," + g.ToString() + "," + b.ToString() + ")"
        ctx.FillStyle <- "rgb(" + r.ToString() + "," + g.ToString() + "," + b.ToString() + ")"
        ctx.BeginPath()
        ctx.Arc(x, y |> flip, float radius, 0., float PI * 2., true)
        ctx.Fill()

    let private drawLine (ctx:CanvasRenderingContext2D) (x : float) (y : float) (angle : float<radians/pi>) (length : float) (width : float) (colour:int * int * int) =
        let r, g, b = colour
        let x_end, y_end = endpoint x y angle length        
        ctx.StrokeStyle <- "rgb(" + r.ToString() + "," + g.ToString() + "," + b.ToString() + ")"
        ctx.LineWidth <- width
        ctx.BeginPath()
        ctx.MoveTo(x, y |> flip)
        ctx.LineTo(x_end, y_end |> flip)
        ctx.Stroke()
        x_end, y_end

    let private calculate iteration (calculation : string) = 
        float(EcmaScript.Global.Eval(calculation.Replace("iteration", (iteration + 1).ToString())).ToString())

    let private colour col calculator =
        match col with
            | Colour.Random -> randomColour()
            | Colour.Defined(red = r;green = g; blue = b) -> int (calculator r), int(calculator g), int (calculator b)

    let rec private drawShapes (ctx:CanvasRenderingContext2D) (x : float) (y : float) (angle : float<radians/pi>) (shapes : list<Shape>) calculator =
        match shapes with
            | s::t -> match s.Type with
                        | Line(width = w;length = l) -> let nextX, nextY = drawLine ctx x y angle (calculator l) (calculator w) <| colour s.Colour calculator
                                                        drawShapes ctx nextX nextY angle t calculator
                        | Circle(radius = rad) -> drawCircle ctx x y (int(calculator rad)) <| colour s.Colour calculator
                                                  drawShapes ctx x y angle t calculator
                        | Rectangle(width = w;height = h) -> drawRectangle ctx x y (calculator w) (calculator h) <| colour s.Colour calculator
                                                             drawShapes ctx x y angle t calculator
            | [] -> x, y

    let rec private processBranches iteration branches angle processAngle =
        match branches with
            | b::t -> processAngle (angle + b.AngleChange)
                      processBranches iteration t angle processAngle
            | [] -> ()

    let rec private processIterations (ctx:CanvasRenderingContext2D) (x : float) (y : float) (iteration : int) (maxIterations : int) (shapes : list<Shape>) (branches : list<Branch>) (angle : float<radians/pi>) =
        let nextX, nextY = drawShapes ctx x y angle shapes (calculate iteration)
        if iteration < maxIterations - 1 then
            let processAngle = processIterations ctx nextX nextY (iteration + 1) maxIterations shapes branches
            processBranches (iteration + 1) branches angle processAngle
        
    let Refresh height width (drawing:Drawing) =
        let element = HTML5.Tags.Canvas[]
        let canvas = As<CanvasElement> element.Dom
        if JavaScript.Get "getContext" canvas = JavaScript.Undefined then
            initialize canvas
        canvas.Height <- height
        canvas.Width <- width
        let context = canvas.GetContext "2d"
        processIterations context drawing.X drawing.Y 0 drawing.MaxIterations drawing.Shapes drawing.Branches drawing.Angle 
        element

