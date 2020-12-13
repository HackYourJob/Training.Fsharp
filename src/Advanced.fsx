// ==== Fsharp Data =====

#r "../packages/FSharp.Data/lib/netstandard2.0/FSharp.Data.dll"

open FSharp.Data

type People = JsonProvider<"""
  [ { "name":"John", "age":94 },
    { "name":"Tomas" } ] """, SampleIsList=true>

let person = People.Parse("""{ "name":"Gustavo" }""")

let age = person.Age

// ====== Operator ======

let sum1to10 = List.fold (+) 0 [1..10]

// ====== rec =======

let rec printItems values =
    match values with
    | [] -> ()
    | head::tail ->
        printfn "%A" head
        printItems tail

printItems [1;2;3]

// ====== contain type ======
[<RequireQualifiedAccess>]
module String10 =
    open System

    type String10 = private String10 of string

    type Error =
        | EmptyValue
        | TooLong

    let create = function
        | value when String.IsNullOrWhiteSpace(value) -> Error EmptyValue
        | value when value.Length > 10 -> Error TooLong
        | value -> String10 value |> Ok

    let get (String10 value) = value

// let a = String10.String10 "a" // not working
let b = String10.create "01234567891"
let c = String10.create "a" |> Result.map String10.get

// ====== interop ======
type ClassB (input) =
    member x.print() = sprintf "%i" input

type ClassA (input) =
    inherit ClassB(input)

    let i = input + 1

    member x.get() = i
    member x.inc() =
        new ClassA(input)

    interface System.IDisposable with
        member x.Dispose() =
            x.print() |> printfn "%s"

let a = ClassB(5)
let start () =
    use b = new ClassA(5)
    b.inc().print()
