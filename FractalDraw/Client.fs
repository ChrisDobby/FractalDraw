namespace FractalDraw

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

[<JavaScript>]
module Client =          
    open Fractals                
    open IntelliFactory.WebSharper.JQueryUI

    let Main () =

        let getExamples show =
            async{
                let! samples = Remoting.Samples()
                return show samples
            } |> Async.Start

        let getDrawings show =
            async {
                let! drawings = Remoting.Drawings()
                return show drawings
            } |> Async.Start

        let saveDrawing drawing show =
            async {
                let! saved = Remoting.Save(drawing)
                return show saved
            } |> Async.Start

        let saved = Div[Attr.Style "height:500px;overflow-y:scroll;overflow-x:hidden"]

        let display = Div[Attr.Style "background-color:lightgray;width:700px;height:550px;float:left"]

        let branch b =
            Div[Attr.Style "margin-left:20px;font-weight:normal"; Text ("Angle change: " + b.AngleChange.ToString())]

        let shape s =
            let shapeTypeElement = function
                | TypeOfShape.Line(w, l) -> Div[] -<
                                            [
                                                Div[Text "Type: Line"]
                                                Div[Text ("Width: " + w)]
                                                Div[Text ("Length: " + l)]
                                            ]
                | TypeOfShape.Circle(r) -> Div[] -<
                                           [
                                                Div[Text "Type: Circle"]
                                                Div[Text ("Radius: " + r)]
                                           ]
                | TypeOfShape.Rectangle(w, h) -> Div[] -<
                                                 [
                                                     Div[Text "Type: Rectangle"]
                                                     Div[Text ("Width: " + w)]
                                                     Div[Text ("Height: " + h)]
                                                 ]

            let colourText = function
                | Colour.Random -> "Random"
                | Colour.Defined(r, g, b) -> "R(" + r.ToString() + ") G(" + g.ToString() + ") B(" + b.ToString() + ")"

            FieldSet[Attr.Style "margin-left:20px;font-weight:normal"] -<
                [
                    shapeTypeElement s.Type
                    Div[Text ("Colour: " + colourText s.Colour)]
                ]

        let definition drawing =
            Div
                [
                    Div[Text ("Iterations: " + drawing.MaxIterations.ToString())]
                    Div[Text ("Start angle: " + drawing.Angle.ToString())]
                    Div[Text ("Start x coord: " + drawing.X.ToString())]
                    Div[Text ("Start y coord: " + drawing.Y.ToString())]                
                    Div[Attr.Style "margin-top:20px; font-weight:bold"; Text "Branches"] -< (drawing.Branches |> List.map (fun b -> branch b))
                    Div[Attr.Style "margin-top:20px; font-weight:bold"; Text "Shapes"] -< (drawing.Shapes |> List.map (fun s -> shape s))
                ]
        
        let refresh update drawing = 
            display.Clear()
            update()
            display.Append(Fractals.Refresh 550 700 drawing)

        let definitionDialog(drawing) =
            let dialogConfig = DialogConfiguration(Title = "Definition")
            Dialog.New(Div[definition drawing], dialogConfig)

        let addSaved (elem : Element) name drawing =
            let drawButton = Button.New "Draw It!"
            drawButton.OnClick(fun _ -> refresh (fun _ -> ()) drawing)

            let viewButton = Button.New "View Parameters"
            viewButton.OnClick(fun _ -> let d = definitionDialog(drawing)
                                        elem.Append(d)
                                        (d :> IPagelet).Render())

            let display = Div[Attr.Style "padding:5px;background-color:lightgray;margin-bottom:10px"] -<
                            [
                                Div[Attr.Style "font-size:110%;font-weight:bold"]  -< [Text name]
                                Div[Attr.Style "margin-top:3px"] -<
                                    [
                                        drawButton
                                        viewButton
                                    ]
                            ]
            elem.Append(display)

        let saveButton drawing =
            let button = Button.New "Save this drawing"
            button.OnClick(fun _ -> saveDrawing drawing (fun a -> let d, n = a
                                                                  addSaved saved n d
                                                                  button.Disable()
                                                                  JavaScript.Alert("Drawing has been saved")))
            button

        let form = Forms.DrawingForm <| fun d -> refresh (fun _ -> display.Append(saveButton d)) d
                                                 

        let formElem = Div[] -< [form]

        let examples = Div[]

        let tabElems = 
            [
                "Examples", examples
                "Saved", saved
                "Draw your own", formElem
            ]

        let tabs = Tabs.New(tabElems, new TabsConfiguration())

        let showExamples (elem : Element) ex =
            let accordionElems = ex |> 
                                 List.map (fun x -> let title, drawing = x
                                                    title, definition drawing)
            let ac = Accordion.New(accordionElems)
            ac.OnActivate(fun _ a -> ex |> 
                                     List.filter (fun x -> let t, _ = x 
                                                           t = a.NewHeader.Text()) |> 
                                     List.iter (fun x -> let _, d = x
                                                         refresh (fun _ -> ()) d))
            elem.Append(ac)

        let showSaved elem saved =
            saved |> List.iter(fun s -> let n, d = s
                                        addSaved elem n d)            

        getExamples (fun s -> examples.Clear()
                              showExamples examples s
                              match s with
                              | h::t -> let t, d = h
                                        refresh (fun _ -> ()) d
                              | [] -> ())

        getDrawings (fun d -> showSaved saved d)

        Div[] -<
        [
            Div[Attr.Style "width:1100px"] -<
            [
                Div[Attr.Style "float:left;width:350px;margin-right:20px"] -< [tabs]
                display
            ]
        ]