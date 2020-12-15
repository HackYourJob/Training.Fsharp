module HYJ.Formation.FizzBuzz

module Implementation =
    let private caseN modulo matchedResult (result, input) =
        if input%modulo = 0
        then (matchedResult, input)
        else (result, input)
    let private caseN' =
        fun modulo ->
            fun matchedResult ->
                fun (result, input) ->
                    if input%modulo = 0
                    then (matchedResult, input)
                    else (result, input)

    let private fizz = caseN 3 "Fizz"
    let private buzz (result, input) = caseN 5 "Buzz" (result, input)
    let private buzz2 x = caseN 5 "Buzz" x

//    let private buzz (result, input) =
//        if input%5 = 0
//        then (result + "Buzz", input)
//        else (result, input)

    type HasResult = bool
    type Result = string
    type Input = int
    type Buzz = (HasResult * Result * Input) -> (HasResult * Result * Input)

    let private simpleNumber = function
        | "", x -> x.ToString()
        | result, _ -> result

    let run v =
        ("", v)
        |> fizz
        |> buzz
        |> simpleNumber

module Tests =
    open Xunit
    open Swensen.Unquote

    open Implementation

    module ``run should`` =

        [<Fact>]
        let ``return number`` () =
            test <@ run 2 = "2" @>

        [<Theory>]
        [<InlineData(3)>]
        [<InlineData(6)>]
        [<InlineData(9)>]
        let ``return Fizz when modulo 3`` value =
            test <@ run value = "Fizz" @>

        [<Theory>]
        [<InlineData(5)>]
        [<InlineData(10)>]
        [<InlineData(20)>]
        let ``return Buzz when modulo 5`` value =
            test <@ run value = "Buzz" @>

        [<Theory>]
        [<InlineData(15)>]
        [<InlineData(30)>]
        [<InlineData(45)>]
        let ``return FizzBuzz when modulo 15`` value =
            test <@ run value = "FizzBuzz" @>
