module HYJ.Formation.TennisKataApi

module Domain =
    type PlayerScore =
        | Love
        | Fifteen
        | Thirty
    type Player = Player1 | Player2
    type Score =
        | Game of Player
        | Advantage of Player
        | Deuce
        | Forty of Player * PlayerScore
        | OtherPoints of PlayerScore * PlayerScore

    let private scoreWhenOtherPoints (player1Score, player2Score) player =
        match player with
        | Player1 ->
            match player1Score with
            | Love -> OtherPoints (Fifteen, player2Score)
            | Fifteen -> OtherPoints (Thirty, player2Score)
            | Thirty -> Forty (Player1, player2Score)
        | Player2 ->
            match player2Score with
            | Love -> OtherPoints (player1Score, Fifteen)
            | Fifteen -> OtherPoints (player1Score, Thirty)
            | Thirty -> Forty (Player2, player1Score)

    let private scoreWhenForty (fortyPlayer, otherPlayerScore) player =
        match player with
        | player when player = fortyPlayer -> Game player
        | _ ->
            match otherPlayerScore with
            | Love -> Forty (fortyPlayer, Fifteen)
            | Fifteen -> Forty (fortyPlayer, Thirty)
            | Thirty -> Deuce

    let private scoreWhenAdvantage advantagePlayer player =
        match player with
        | p when p = advantagePlayer -> Game p
        | _ -> Deuce

    let scoreAPoint player previousScore =
        match previousScore with
        | OtherPoints (player1Score, player2Score) -> scoreWhenOtherPoints (player1Score, player2Score) player
        | Forty (p, otherPlayerScore) -> scoreWhenForty (p, otherPlayerScore) player
        | Deuce -> Advantage player
        | Advantage p -> scoreWhenAdvantage p player
        | Game _ -> previousScore

module DataAccess =
    open System.Collections.Generic

    module GamesStore =
        type GameId = string
        type private Store = Dictionary<GameId, Domain.Score>
        type Infra = {
            Store: GameId -> Domain.Score -> Async<unit>
            GetScore: GameId -> Domain.Score option Async
        }

        let store (games: Store) id score =
            games.[id] <- score
            async.Return ()

        let getScore (games: Store) id =
            match games.TryGetValue id with
            | true, score -> Some score
            | false, _ -> None
            |> async.Return

        let createInfra () =
            let games = Dictionary<_, _>()
            {
                Store = store games
                GetScore = getScore games
            }

module Api =
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.DependencyInjection
    open Giraffe
    open FSharp.Control.Tasks.V2.ContextInsensitive
    open Microsoft.AspNetCore.Http

    let values = seq {
        yield 5
        yield! [2; 3]
    }

    let toto value x =
        value x

    let getScore (getInfra: HttpContext -> DataAccess.GamesStore.Infra) id next (ctx: HttpContext) =
        task {
            let gameStore = getInfra ctx

            let! score = gameStore.GetScore id
            let result =
                match score with
                | None -> RequestErrors.notFound (text "game not exists")
                | Some score ->
                    score
                    |> json
                    |> Successful.ok

            return! result next ctx
        }

    let webApp getInfra =
        choose [
            GET >=> choose [
                routef "/games/%s" (fun id -> getScore getInfra id)
                route "/"       >=> text "Hello"
            ]
            POST >=> choose [
            ]
        ]

    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe (webApp (fun ctx -> ctx.GetService<DataAccess.GamesStore.Infra>()))

    let configureServices (services : IServiceCollection) =
        let gameStore = DataAccess.GamesStore.createInfra ()
        gameStore.Store "A" Domain.Deuce |> Async.RunSynchronously
        services.AddSingleton<_>(gameStore) |> ignore

        services.AddGiraffe() |> ignore

    let start () =
        Host.CreateDefaultBuilder()
            .ConfigureWebHostDefaults(fun webHostBuilder ->
                    webHostBuilder
                        .Configure(configureApp)
                        .ConfigureServices(configureServices)
                        |> ignore
            )
            .Build()
            .Run()

module Tests =
    open Xunit
    open Swensen.Unquote

    module Helper =
        open System
        open System.IO
        open System.Text
        open Giraffe.Serialization
        open Newtonsoft.Json
        open FSharp.Control.Tasks.V2.ContextInsensitive
        open Microsoft.AspNetCore.Http

        type private RequestServicesFactory() =
            interface IServiceProvider with
                  member this.GetService(serviceType) =
                      match serviceType.Name with
                      | "IJsonSerializer" ->
                          NewtonsoftJsonSerializer(JsonSerializerSettings()) :> obj
                      | unknown -> failwithf "Unknown service: %s" unknown

        let getBody (response: HttpResponse) =
            response.Body.Position <- 0L
            use reader = new StreamReader(response.Body, Encoding.UTF8)
            reader.ReadToEnd()

        type private Method = GET | POST

        let private idHandler context = task { return Some context }

        let private createContext (method: Method) path =
            let context = DefaultHttpContext()
            context.RequestServices <- RequestServicesFactory()
            context.Response.Body <- new MemoryStream()

            let uri = Uri("http://localhost" + path)
            context.Request.Method <- sprintf "%A" method
            context.Request.Path <- PathString(uri.AbsolutePath)
            context.Request.QueryString <- QueryString(uri.Query)
            context :> HttpContext

        let private setRequestBody (context: HttpContext) content =
            let data =
                JsonConvert.SerializeObject(content)
                |> Encoding.UTF8.GetBytes
            context.Request.Body <- new MemoryStream(data)

        let private run infra method path body =
            let context = createContext method path
            match body with
            | Some content -> setRequestBody context content
            | None -> ()

            let result = Api.webApp (fun _ -> infra) idHandler context |> Async.AwaitTask |> Async.RunSynchronously

            test <@ result = Some context @>

            context.Response

        let get infra path =
            run infra GET path None

    open Helper
    open Domain

    [<Fact>]
    let ``return game status if exists when getStatus`` () =
        let infra: DataAccess.GamesStore.Infra = {
            Store = fun _ -> failwith "Not expected"
            GetScore = function
                | "A" -> Some Deuce |> async.Return
                | _ -> None |> async.Return
        }

        let response = get infra "/games/A"

        test <@ response.StatusCode = 200 @>
        let content = getBody response
        test <@ content = """{"Case":"Deuce"}""" @>
