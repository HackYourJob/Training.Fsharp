module HYJ.Formation.FizzBuzz

module Implementation =
    let run v =
        v.ToString()


module Tests =
    open Xunit
    open Swensen.Unquote

    open Implementation

    module ``run should`` =

    [<Fact>]
    let ``return number`` () =
        test <@ run 2 = "2" @>

    [<Fact>]
    [<InlineData(3)>]
    [<InlineData(6)>]
    [<InlineData(9)>]
    let ``return fizz when modulo 3`` value =
        test <@ run value = "Fizz" @>
