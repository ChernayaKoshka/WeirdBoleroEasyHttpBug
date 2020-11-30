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
open System.IO
open Microsoft.FSharp.Reflection
open FSharp.Reflection
open System.Text.Json
open EasyHttp.Serializers
open System.Text
open System.IO

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
| DynamicTaskPage
| SendDirectPage
| SendIndirectPage

type Model =
    {
        api: Api
        client: HttpClient
        page: Page
        pageText: string
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
        pageText = "Click any links below"
    }, Cmd.none

let test() = task {
    do! Task.Delay(750)
    return 5
}
let makeFunc() =
    FSharpValue.MakeFunction(typeof<unit -> Task<int>>, fun arg -> (arg :?> unit) |> test :> obj)
let dynamicallyCreatedTask = makeFunc() :?> unit -> Task<int>

// #3 Fix: calling send directly
let send (client: HttpClient) (method: HttpMethod) (serializationType: SerializationType) (requestUri: Uri) (uriFragment: string) (content: obj) : Task<'ReturnType> = task {
    let! response =
        match serializationType with
        | JsonSerialization ->
            let requestUri = Uri(requestUri, uriFragment)
            // TODO: Allow JsonSerializer to house serialization options? How would that work with an Attribute?
            let content = JsonSerializer.Serialize(content)
            new HttpRequestMessage(method, requestUri,
                Content = new StringContent(content, Encoding.UTF8, "application/json")
            )
        | PathStringSerialization ->
            let uriFragment =
                match PathString.serialize uriFragment content with
                | Ok fragment -> fragment
                | Error err -> failwith err
            let requestUri = Uri(requestUri, uriFragment)
            new HttpRequestMessage(method, requestUri)
        |> client.SendAsync
    if typeof<'ReturnType> = typeof<unit> then
        return box () :?> 'ReturnType

    else
    let! stream = response.Content.ReadAsStreamAsync()
    if typeof<'ReturnType> = typeof<string> then
        use reader = new StreamReader(stream)
        let! body = reader.ReadToEndAsync()
        return box body :?> 'ReturnType

    else
    return! JsonSerializer.DeserializeAsync<'ReturnType>(stream)
}

let sendDirect client =  send client HttpMethod.Get PathStringSerialization Api.BaseUri "" (box null)

type Message =
    | SetPage of Page
    | LaunchEasyHttpTask
    | LaunchDirectClientTask
    | LaunchEasyHttpWrappedTask
    | LaunchDynamicTask
    | LaunchSendDirectTask
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
            | DynamicTaskPage ->
                Cmd.ofMsg LaunchDynamicTask
            | SendDirectPage ->
                Cmd.ofMsg LaunchSendDirectTask
            | _ ->
                Cmd.none
        { model with
            page = page
            pageText = ""
        }, nextCmd
    | LaunchEasyHttpTask ->
        model, Cmd.OfTask.either model.api.test () (string >> SetText) (string >> SetText)
    | LaunchEasyHttpWrappedTask ->
        model, Cmd.OfTask.either (fun () -> task { return! model.api.test () }) () (string >> SetText) (string >> SetText)
    | LaunchDirectClientTask ->
        model, Cmd.OfTask.either directClientTask model.client (string >> SetText) (string >> SetText)
    | LaunchDynamicTask ->
        model, Cmd.OfTask.either dynamicallyCreatedTask () (string >> SetText) (string >> SetText)
    | LaunchSendDirectTask ->
        model, Cmd.OfTask.either sendDirect model.client (string >> SetText) (string >> SetText)
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
            button [ on.click (fun _ -> dispatch LaunchEasyHttpTask) ] [ text "LaunchEasyHttpTask Directly" ]
            button [ on.click (fun _ -> dispatch LaunchEasyHttpWrappedTask) ] [ text "LaunchEasyHttpWrappedTask Directly" ]
            button [ on.click (fun _ -> dispatch LaunchDirectClientTask) ] [ text "LaunchDirectClientTask Directly" ]
            button [ on.click (fun _ -> dispatch LaunchDynamicTask) ] [ text "LaunchDynamicTask Directly" ]
            button [ on.click (fun _ -> dispatch LaunchSendDirectTask) ] [ text "LaunchSendDirectTask Directly" ]
        ]
        if model.page = Home then text "blah"
        hr [ ]
        p [ ] [ a [ router.HRef Home ] [ text "Home" ] ]

        // clicking this will cause an error in the console and the task result not to be sent out
        // navigating to /OtherPage directly doesn't cause this issue
        p [ ] [ a [ router.HRef EasyHttpPage ] [ text "EasyHttpPage (breaks, error in console on FireFox)" ] ]

        // not these, though?
        p [ ] [ a [ router.HRef EasyHttpWrappedPage ] [ text "EasyHttpWrappedPage" ] ]
        p [ ] [ a [ router.HRef DirectClientPage ] [ text "DirectClientPage" ] ]
        p [ ] [ a [ router.HRef DynamicTaskPage ] [ text "DynamicTaskPage" ] ]
        p [ ] [ a [ router.HRef SendDirectPage ] [ text "SendDirectPage" ] ]
    ]


type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let hcf = this.Services.GetService<IHttpClientFactory>()
        Program.mkProgram (fun _ -> initModel hcf) update view
        |> Program.withConsoleTrace
        |> Program.withRouter router
