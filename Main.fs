module MinRepro.Client.Main

open Elmish
open Bolero
open Bolero.Html
open System.Threading.Tasks
open System
open Microsoft.FSharp.Reflection

// using TaskBuilder.fs over Ply fixes the issue
open FSharp.Control.Tasks.NonAffine //.V2.ContextInsensitive

type FunctionHolder private () =
    static member Test (arg1: obj) : Task<string> = task {
        return (string arg1)
    }

// being `inline` somehow contributes to the issue
let inline makeBreakingFunction (testArg: Uri) =
    let returnType = typeof<unit -> Task<string>>
    FSharpValue.MakeFunction(
        returnType,
        fun arg ->
            // If the last argument passed in is `null`, you get an `Uncaught Error: undefined` exception in FireFox
            // dynamic invocation is the key here, it seems.

            let res = typeof<FunctionHolder>.GetMethod(nameof FunctionHolder.Test).Invoke(null, [| null |])
            // let res = typeof<Http>.GetMethod(nameof Http.Send).Invoke(null, [| "I'm not null!" |])

            // if you uncomment the next line it inexplicably fixes the `Uncaught Error: undefined` exception in FireFox even if the last argument is null
            // let arbitrary = Math.Abs(2)
            box res
    )
    :?> unit -> Task<string>

type Page =
| Home
| EasyHttpPage

type Model =
    {
        breaks: unit -> Task<string>
        page: Page
        pageText: string
    }

type Message =
    | SetPage of Page
    | LaunchBreakingTask
    | SetText of string

let initModel =
    let breaks = makeBreakingFunction (Uri("https://blah.com"))

    {
        breaks = breaks
        page = Home
        pageText = "Click any links below"
    }, Cmd.none


let router = Router.infer SetPage (fun m -> m.page)

let update message model =
    match message with
    | SetPage page ->
        let nextCmd =
            match page with
            | EasyHttpPage ->
                Cmd.ofMsg LaunchBreakingTask
            | _ ->
                Cmd.none
        { model with
            page = page
            pageText = ""
        }, nextCmd
    | LaunchBreakingTask ->
        model, Cmd.OfTask.either model.breaks () (string >> SetText) (string >> SetText)
    | SetText str ->
        { model with
            pageText = str
        }, Cmd.none

let view model dispatch =
    div [ ] [
        p [ ] [
            text model.pageText
        ]
        p [ ] [
            button [ on.click (fun _ -> dispatch LaunchBreakingTask) ] [ text "LaunchBreakingTask Directly" ]
        ]
        if model.page = Home then text "blah"
        hr [ ]
        p [ ] [ a [ router.HRef Home ] [ text "Home" ] ]

        // clicking this will cause an error in the console and the task result not to be sent out
        // navigating to /OtherPage directly doesn't cause this issue
        p [ ] [ a [ router.HRef EasyHttpPage ] [ text "LaunchBreakingTask (breaks, error in console on FireFox no issue Edge/Chromium)" ] ]
    ]


type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel) update view
        |> Program.withConsoleTrace
        |> Program.withRouter router
