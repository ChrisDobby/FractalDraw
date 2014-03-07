namespace FractalDraw

open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

type Action =
    | Home

module Controls =

    [<Sealed>]
    type EntryPoint() =
        inherit Web.Control()

        [<JavaScript>]
        override __.Body =
            Client.Main() :> _

module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Content.HtmlElement>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Title = title
                Body = body context
            }

module Site =
    let HomePage =
        Skin.WithTemplate "Draw!" <| fun ctx ->
            [
                Div [new Controls.EntryPoint()]
            ]

    let Main = Sitelet.Content "/" Home HomePage
        

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home]

type Global() =
    inherit System.Web.HttpApplication()

    member g.Application_Start(sender: obj, args: System.EventArgs) =
        ()

[<assembly: Website(typeof<Website>)>]
do ()