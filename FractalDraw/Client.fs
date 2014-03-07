namespace FractalDraw

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

[<JavaScript>]
module Client =          
    open Fractals                
    open IntelliFactory.WebSharper.JQueryUI

    let Main () =
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

        let example drawing =
            Div
                [
                    Div[Text ("Iterations: " + drawing.MaxIterations.ToString())]
                    Div[Text ("Start angle: " + drawing.Angle.ToString())]
                    Div[Text ("Start x coord: " + drawing.X.ToString())]
                    Div[Text ("Start y coord: " + drawing.Y.ToString())]                
                    Div[Attr.Style "margin-top:20px; font-weight:bold"; Text "Branches"] -< (drawing.Branches |> List.map (fun b -> branch b))
                    Div[Attr.Style "margin-top:20px; font-weight:bold"; Text "Shapes"] -< (drawing.Shapes |> List.map (fun s -> shape s))
                ]

        let refresh drawing = 
            display.Clear()
            display.Append(Fractals.Refresh 550 700 drawing)

        let form = Forms.DrawingForm <| refresh

        let formElem = Div[] -< [form]

        let getExamples show =
            async{
                let! samples = Remoting.Samples()
                return show samples
            } |> Async.Start

        let examples = Div[]

        let tabElems = 
            [
                "Examples", examples
                "Draw your own", formElem
            ]

        let tabs = Tabs.New(tabElems, new TabsConfiguration())

        let showExamples (elem : Element) ex =
            let accordionElems = ex |> 
                                 List.map (fun x -> let title, drawing = x
                                                    title, example drawing)
            let ac = Accordion.New(accordionElems)
            ac.OnActivate(fun _ a -> ex |> 
                                     List.filter (fun x -> let t, _ = x 
                                                           t = a.NewHeader.Text()) |> 
                                     List.iter (fun x -> let _, d = x
                                                         refresh d))
            elem.Append(ac)

        getExamples (fun s -> examples.Clear()
                              showExamples examples s
                              match s with
                              | h::t -> let t, d = h
                                        refresh d
                              | [] -> ())

        Div[] -<
        [
            Div[Attr.Style "width:1100px"] -<
            [
                Div[Attr.Style "float:left;width:350px;margin-right:20px"] -< [tabs]
                display
            ]
        ]