module MinRepro.Client.Main

open Elmish
open Bolero
open Bolero.Html
open System.Threading.Tasks
open FSharp.Control.Tasks
open System
open EasyHttp
open System.Net.Http
open Microsoft.Extensions.DependencyInjection

type Response =
    {
        Method: string
        Path: string
        QueryString: string
        Content: string
    }

type Api =
    {
        [<Method("GET")>]
        test: unit -> Task<string>
    }
    with
        static member BaseUri = Uri("https://jsonplaceholder.typicode.com/users")

type Page =
| Home
| EasyHttpPage
| EasyHttpWrappedPage
| DirectClientPage

type Model =
    {
        api: Api
        client: HttpClient
        page: Page
        x: string
    }

let initModel (hcf: IHttpClientFactory) =
    let client = hcf.CreateClient()
    let api =
        match makeApi<Api> client with
        | Ok api -> api
        | Error err -> failwith err

    {
        api = api
        client = client
        page = Home
        x = "Press the button"
    }, Cmd.none
    
type Message =
    | SetPage of Page
    | LaunchEasyHttpTask
    | LaunchDirectClientTask
    | LaunchEasyHttpWrappedTask
    | SetText of string

let router = Router.infer SetPage (fun m -> m.page)

let directClientTask (hc: HttpClient) = task {
    let! response = hc.GetAsync(Uri("https://jsonplaceholder.typicode.com/users"))
    let! stream = response.Content.ReadAsStreamAsync()
    use reader = new System.IO.StreamReader(stream)
    return! reader.ReadToEndAsync()
}

let update message model =
    match message with
    | SetPage page ->
        let nextCmd = 
            match page with
            | EasyHttpPage -> 
                Cmd.ofMsg LaunchEasyHttpTask
            | EasyHttpWrappedPage -> 
                Cmd.ofMsg LaunchEasyHttpWrappedTask
            | DirectClientPage -> 
                Cmd.ofMsg LaunchDirectClientTask
            | _ -> 
                Cmd.none
        { model with
            page = page
            x = ""
        }, nextCmd
    | LaunchEasyHttpTask ->
        model, Cmd.OfTask.either model.api.test () (string >> SetText) (string >> SetText)
    | LaunchEasyHttpWrappedTask ->
        model, Cmd.OfTask.either (fun () -> task { return! model.api.test () }) () (string >> SetText) (string >> SetText)
    | LaunchDirectClientTask ->
        model, Cmd.OfTask.either directClientTask model.client (string >> SetText) (string >> SetText)
    | SetText str -> 
        { model with 
            x = str 
        }, Cmd.none

let view model dispatch =
    div [ ] [
        p [ ] [ 
            text model.x 
        ]
        p [ ] [ 
            button [ on.click (fun _ -> dispatch LaunchEasyHttpTask) ] [ text "LaunchEasyHttpTask Directly" ]
            button [ on.click (fun _ -> dispatch LaunchEasyHttpWrappedTask) ] [ text "LaunchEasyHttpWrappedTask Directly" ]
            button [ on.click (fun _ -> dispatch LaunchDirectClientTask) ] [ text "LaunchDirectClientTask Directly" ]
        ]
        if model.page = Home then text "blah"
        hr [ ]
        p [ ] [ a [ router.HRef Home ] [ text "Home" ] ]

        // clicking this will cause an error in the console and the task result not to be sent out
        // navigating to /OtherPage directly doesn't cause this issue
        p [ ] [ a [ router.HRef EasyHttpPage ] [ text "EasyHttpPage" ] ]

        // not this, though?
        p [ ] [ a [ router.HRef EasyHttpWrappedPage ] [ text "EasyHttpWrappedPage" ] ]

        // not this, though?
        p [ ] [ a [ router.HRef DirectClientPage ] [ text "DirectClientPage" ] ]
    ]
    

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let hcf = this.Services.GetService<IHttpClientFactory>()
        Program.mkProgram (fun _ -> initModel hcf) update view
        |> Program.withConsoleTrace
        |> Program.withRouter router
