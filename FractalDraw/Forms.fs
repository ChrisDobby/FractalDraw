namespace FractalDraw

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

[<JavaScript>]
module Forms = 
    open IntelliFactory.WebSharper.Formlet
    open IntelliFactory.WebSharper.Formlet.Controls
    open Fractals

    let BranchFormlet : Formlet<Branch> =
        let angleF = Input "" |>
                     Validator.IsNotEmpty "Angle change required" |>
                     Validator.IsFloat "Please enter a number" |>
                     Enhance.WithValidationIcon |>
                     Enhance.WithTextLabel "Angle Change"

        Formlet.Yield (fun a -> {AngleChange = float a * 1.<radians/pi>})
        <*> angleF

    let DefinedColourFormlet =
        let redF = Input "" |>
                   Validator.IsNotEmpty "Red is required" |>
                   Enhance.WithValidationIcon |>
                   Enhance.WithTextLabel "Red"

        let greenF = Input "" |>
                     Validator.IsNotEmpty "Green is required" |>
                     Enhance.WithValidationIcon |>
                     Enhance.WithTextLabel "Green"

        let blueF = Input "" |>
                    Validator.IsNotEmpty "Blue is required" |>
                    Enhance.WithValidationIcon |>
                    Enhance.WithTextLabel "Blue"

        Formlet.Yield (fun r g b -> Colour.Defined(r, g, b))
        <*> redF
        <*> greenF
        <*> blueF

    let RandomColourFormlet : Formlet<Colour> = 
        Formlet.Yield (Colour.Random)

    type typeOfColour = | Random | Defined
    let ColourFormlet : Formlet<Colour> =
        let colourTypeF = Select 0 [
                                        ("Random", Random);
                                        ("Define", Defined)
                                   ] |>
                          Enhance.WithTextLabel "Colour"
        Formlet.Do {
            let! colourType = colourTypeF
            return!
                match colourType with
                    | Random -> RandomColourFormlet
                    | Defined -> DefinedColourFormlet
        }

    let LineFormlet =
        let widthF = Input "" |>
                     Validator.IsNotEmpty "Width is required" |>
                     Enhance.WithValidationIcon |>
                     Enhance.WithTextLabel "Width"

        let lengthF = Input "" |>
                      Validator.IsNotEmpty "Length is required" |>
                      Enhance.WithValidationIcon |>
                      Enhance.WithTextLabel "Length"
       
        let colourF = ColourFormlet

        Formlet.Yield (fun w l c -> {Type = Line(w, l); Colour = c})
        <*> widthF
        <*> lengthF
        <*> colourF

    let CircleFormlet =
        let radiusF = Input "" |>
                      Validator.IsNotEmpty "Radius is required" |>
                      Enhance.WithValidationIcon |>
                      Enhance.WithTextLabel "Radius"
       
        let colourF = ColourFormlet

        Formlet.Yield (fun r c -> {Type = Circle(r); Colour = c})
        <*> radiusF
        <*> colourF

    let RectangleFormlet =
        let widthF = Input "" |>
                     Validator.IsNotEmpty "Width is required" |>
                     Enhance.WithValidationIcon |>
                     Enhance.WithTextLabel "Width"

        let heightF = Input "" |>
                      Validator.IsNotEmpty "Height is required" |>
                      Enhance.WithValidationIcon |>
                      Enhance.WithTextLabel "Length"
       
        let colourF = ColourFormlet

        Formlet.Yield (fun w h c -> {Type = Rectangle(w, h); Colour = c})
        <*> widthF
        <*> heightF    
        <*> colourF

    type typeOfShape = | Line | Circle | Rectangle
    let ShapeFormlet : Formlet<Shape> =
        let shapeTypeF = Select 0 [
                                    ("Line", Line);
                                    ("Circle", Circle);
                                    ("Rectangle", Rectangle)
                                  ]

        Formlet.Do{
            let! shapeType = shapeTypeF
            return!
                match shapeType with
                    | Line -> LineFormlet
                    | Circle -> CircleFormlet
                    | Rectangle -> RectangleFormlet
        }

    let DrawingFormlet : Formlet<Drawing> =
        let iterationsF = Input "" |>
                          Validator.IsNotEmpty "Number of iterations required" |>
                          Validator.IsInt "Please enter a whole number" |>
                          Enhance.WithValidationIcon |>
                          Enhance.WithTextLabel "Iterations"
        
        let angleF = Input "" |>
                     Validator.IsNotEmpty "Start angle required" |>
                     Validator.IsFloat "Please enter a number" |>
                     Enhance.WithValidationIcon |>
                     Enhance.WithTextLabel "Start angle"

        let startXF = Input "350" |>
                      Validator.IsNotEmpty "Start x coord required" |>
                      Validator.IsFloat "Please enter a number" |>
                      Enhance.WithValidationIcon |>
                      Enhance.WithTextLabel "Start x coord"

        let startYF = Input "250" |>
                      Validator.IsNotEmpty "Start y coord required" |>
                      Validator.IsFloat "Please enter a number" |>
                      Enhance.WithValidationIcon |>
                      Enhance.WithTextLabel "Start y coord"

        let branchesF = (BranchFormlet |> Enhance.WithLegend "Branch") |>
                        Enhance.Many |>
                        Enhance.WithLegend "Branches"

        let shapesF = (ShapeFormlet |> Enhance.WithLegend "Shape") |>
                      Enhance.Many |>
                      Enhance.WithLegend "Shapes"

        Formlet.Yield (fun iterations angle startX startY branches shapes -> 
        {
            MaxIterations = int iterations; 
            Angle = float angle * 1.<radians/pi>; 
            X = (float startX); 
            Y = float startY;
            Branches = branches;
            Shapes = shapes
        })
        <*> iterationsF
        <*> angleF
        <*> startXF
        <*> startYF
        <*> branchesF
        <*> shapesF
        |> Enhance.WithCustomSubmitAndResetButtons
                            {
                                Enhance.FormButtonConfiguration.Default with
                                    Label = Some "Draw!"
                            }
                            {
                                Enhance.FormButtonConfiguration.Default with
                                    Label = Some "Reset"
                            }
        |> Enhance.WithFormContainer
    
    let DrawingForm draw =
        let fc = {
            Enhance.FormContainerConfiguration.Default with
                Header = "Define drawing" |> Enhance.FormPart.Text |> Some
                Description = "Set up parameters for drawing and click the Draw button" |> Enhance.FormPart.Text |> Some
        }

        let f = DrawingFormlet |>
                Enhance.WithCustomFormContainer fc

        Formlet.Run (fun drawing -> draw drawing) f
