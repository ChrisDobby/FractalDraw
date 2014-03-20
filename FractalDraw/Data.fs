namespace FractalDraw

open System.Data.Linq
open Microsoft.FSharp.Data.TypeProviders
open System.ComponentModel.DataAnnotations
open FSharp.Data
open Fractals

type internal dbSchema = SqlEntityConnection<ConnectionStringName = "DrawConnection", Pluralize = true, ConfigFile = "web.config">

module Data =
    let internal getDbShape = function
        | TypeOfShape.Line(w, l) -> new dbSchema.ServiceTypes.Shape(Type = 0, Width = w, Length = l)
        | TypeOfShape.Rectangle(w, l) -> new dbSchema.ServiceTypes.Shape(Type = 1, Width = w, Length = l)
        | TypeOfShape.Circle(r) -> new dbSchema.ServiceTypes.Shape(Type = 2, Radius = r)

    let internal getDbColour = function
        | Colour.Random -> true, "", "", ""
        | Colour.Defined(r, g, b) -> false, r, g, b

    let internal getShapeType = function
        | 0, w, l, _ -> TypeOfShape.Line(w, l)
        | 1, w, l, _ -> TypeOfShape.Rectangle(w, l)
        | 2, _, _, r -> TypeOfShape.Circle(r)
        | _, _, _, _ -> TypeOfShape.Line("", "")

    let internal getColour rnd r g b =
        if rnd then
            Colour.Random
        else
            Colour.Defined(r, g, b)

    let addDrawing drawing name =
        let db = dbSchema.GetDataContext()
        let newDrawing = dbSchema.ServiceTypes.Drawing.CreateDrawing(
                                                                        0,
                                                                        drawing.MaxIterations,
                                                                        float drawing.Angle,
                                                                        drawing.X,
                                                                        drawing.Y
                                                                    )

        newDrawing.Name <- name
        db.Drawings.AddObject(newDrawing)
        drawing.Branches 
            |> Seq.map(fun b -> new dbSchema.ServiceTypes.Branch(AngleChange = float b.AngleChange))
            |> Seq.iter(fun b -> newDrawing.Branches.Add(b))
        drawing.Shapes
            |> Seq.mapi(fun i s -> let shape = getDbShape s.Type
                                   let rnd, r, g, b = getDbColour s.Colour
                                   shape.DisplayOrder <- i
                                   shape.RandomColour <- rnd
                                   shape.Red <- r
                                   shape.Green <- g
                                   shape.Blue <- b
                                   shape
                       )
            |> Seq.iter(fun s -> newDrawing.Shapes.Add(s))

        db.DataContext.SaveChanges() |> ignore

        drawing

    let all =
        let db = dbSchema.GetDataContext()
        query { for drawing in db.Drawings do select (drawing)} |> Seq.toList
            |> List.map(fun d -> d.Name, {
                                                    MaxIterations = d.Iterations
                                                    Angle = d.Angle * 1.<radians/pi>
                                                    X = d.StartX
                                                    Y = d.StartY
                                                    Shapes = d.Shapes |> Seq.map(fun s -> { 
                                                                                            Type = getShapeType(s.Type, s.Width, s.Length, s.Radius)
                                                                                            Colour = getColour s.RandomColour s.Red s.Green s.Blue
                                                                                          }
                                                                                ) |> Seq.toList
                                                    Branches = d.Branches |> Seq.map(fun b -> { AngleChange = b.AngleChange * 1.<radians/pi> }) |> Seq.toList
                                                })
