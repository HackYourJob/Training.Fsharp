open System
open HYJ.Formation

let succeedExitCode = 0

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    TennisKataApi.Api.start ()

    succeedExitCode
