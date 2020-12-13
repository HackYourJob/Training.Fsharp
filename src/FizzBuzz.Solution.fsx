type FizzBuzzResult =
    | Matched of string
    | Unmatched of int

let handleNcase divisor result = function
    | Unmatched n when n%divisor = 0 -> Matched result
    | other -> other

let handle15case = handleNcase 15 "FizzBuzz"
let handle5case = handleNcase 5 "Buzz"
let handle3case = handleNcase 5 "Fizz"
let finalStep = function
    | Unmatched n -> n |> sprintf "%d"
    | Matched v -> v

let fizzBuzz n =
    Unmatched n
    |> handle15case
    |> handle3case
    |> handle5case
    |> finalStep

// test it on the numbers up to 30
[1..30] |> List.map fizzBuzz

