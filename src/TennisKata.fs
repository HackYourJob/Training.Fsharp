module HYJ.Formation.TennisKata

module Implementation =
    let run v =
        v.ToString()


module Tests =
    open Xunit
    open Swensen.Unquote

    open Implementation

    [<Fact>]
    let ``should return number`` () =
        test <@ run 2 = "2" @>
