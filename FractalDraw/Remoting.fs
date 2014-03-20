namespace FractalDraw

open IntelliFactory.WebSharper
open Fractals

module Remoting =
    [<Remote>]
    let Samples() = 
        async {
            return
                [("Example 1", 
                    {
                        MaxIterations = 15; 
                        Angle = 0.5<radians/pi>; 
                        X = 350.; 
                        Y = 10.;
                        Branches = [
                                        {AngleChange = 0.1<radians/pi>};
                                        {AngleChange = -0.1<radians/pi>}
                                   ];
                        Shapes = [
                                    {
                                        Type = Fractals.Line("14 - iteration", "60 - (iteration * 4)");
                                        Colour = Fractals.Colour.Defined("0", "iteration * 13", "0")
                                    }
                                 ]
                    }
                );
                ("Example 2", 
                    {
                        MaxIterations = 30; 
                        Angle = 0.3<radians/pi>; 
                        X = 200.; 
                        Y = 200.;
                        Branches = [
                                        {AngleChange = -0.4<radians/pi>};
                                   ];
                        Shapes = [
                                    {
                                        Type = Fractals.Line("10", "200 - (iteration * 6)");
                                        Colour = Fractals.Colour.Defined("0", "0", "0")
                                    };
                                    {
                                        Type = Fractals.Circle("10");
                                        Colour = Fractals.Colour.Defined("0", "0", "0")
                                    };
                                 ]
                    }
                );
                ("Example 3", 
                    {
                        MaxIterations = 15; 
                        Angle = 0.5<radians/pi>; 
                        X = 350.; 
                        Y = 100.;
                        Branches = [
                                        {AngleChange = 0.1<radians/pi>};
                                        {AngleChange = -0.15<radians/pi>}
                                   ];
                        Shapes = [
                                    {
                                        Type = Fractals.Line("17 - iteration", "60 - (iteration * 4)");
                                        Colour = Fractals.Colour.Random
                                    }
                                 ]
                    }
                );                
            ]

        }

    [<Remote>]
    let Drawings() =
        async {
            return Data.all
        }

    [<Remote>]
    let Save(drawing) =
        async{
            let name = System.DateTime.Now.ToString("yyyy-MM-dd hh:mm")
            let added = Data.addDrawing drawing name
            return added, name
        }
