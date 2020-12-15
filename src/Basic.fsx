// variable
let a: string = "Hello"

// inference
let b = "hello"

// immutable
b = "World"
//b <- "World" // not working

let mutable c = "bad"
c <- "good"

// function
let add1 x = x + 1
// val add1 : x:int -> int
// val add1 : x:int * y:'a -> int
let add2 (x: int) : int =
    let y = add1 x
    add1 y

add1 5
add2 5

// module
module m1 =
    let add x = x + 1

module m2 =
    let add' (x: int) : int =
        let y = m1.add x
        m1.add y

    open m1

    let add'' (x: int) : int =
        let y = add x
        add y

m2.add' 5

[1; 2; 3]
|> Seq.map (fun i -> i + 1)
|> Seq.toList

let (head::tail) = [1; 2; 3]
1::[2; 3]
[1]@[2;3]

[| 1; 2; 3 |]

[1..5]

let a x =
    [
        let b = x
        yield b + 2
        yield b + 3
    ]

Map.ofList [("a", 5); ("v", 6)]
Map.ofList ["a", 5; "v", 6]
Map.ofList [
    "a", 5
    "v", 6
    "a", 6
]

Set.ofList [1; 2; 1]

module Domain =
    open System

//type Name = string
    type Name = private Name of string
    type NameError = | NameIsEmpty | TooLong of string
    module Name =
        let create value =
            match value with
            | value when String.IsNullOrWhiteSpace(value) -> NameIsEmpty |> Error
            | value -> Name value |> Ok

        let get (Name name) = name

open Domain

type PersonContact = {
    Name: Name
}
//{ Name = Name "joe" }
match Name.create "joe" with
| Ok name ->
    match Name.create (Name.get name + "bob") with
    | Ok name -> Ok { Name = name }
    | Error error -> Error error
| Error error -> Error error

let bind apply result =
    match result with
    | Ok value -> apply value
    | Error error -> Error error

let map apply result =
    bind (apply >> Ok) result
    bind (fun result -> apply result |> Ok) result

type AgeError = | IsTooOld
type GlobalError = NameError of NameError | AgeError of AgeError

Name.create "joe"
|> Result.bind (fun name ->
    (Name.get name) + "bob" |> Name.create
)
|> Result.mapError NameError
//|> Result.mapError (fun error -> NameError error)


let add1 x = x + 1
let print x = sprintf "%i" x

add1 5
|> print

let addPrint = add1 >> print
let addPrint x =
    add1 x |> print
addPrint 5

[1; 2]
//|> List.map (fun x -> add1 x |> print)
|> List.map ((+) 5)
