module HYJ.Formation.TennisKata

module Implementation =
    type Player = Player1 | Player2
    type Point =
        | Love
        | Fifteen
        | Thirty
    type Score =
        | Points of Point * Point
        | Fourty of Player * Point
        | Deuce
        | Advantage of Player
        | Game of Player
    type OldScore = Score
    type Play = OldScore -> Player -> Score

    let play oldScore player =
        match player with
        | Player1 -> (Fifteen, Love)
        | Player2 -> (Fifteen, Fifteen)

module Tests =
    open Xunit
    open Swensen.Unquote

    open Implementation

    [<Fact>]
    let ``should return Fifteen Love when Player1 win``() =
        test <@ play (Love, Love) Player1 = (Fifteen, Love) @>

    [<Fact>]
    let ``should return Fifteen Fifteen when Player2 win and previous score Fifteen Love``() =
        test <@ play (Fifteen, Love) Player2 = (Fifteen, Fifteen) @>
