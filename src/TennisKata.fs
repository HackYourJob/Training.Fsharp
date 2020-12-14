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

    let play (oldScore:OldScore) (player:Player) :Score = 
        match player with
        | Player1 ->
            match oldScore with
            | Points (Love, player2Points)  -> Points (Fifteen, player2Points)
            | Points (Fifteen, player2Points) -> Points (Thirty, player2Points)
            | Points (Thirty, player2Points) -> Fourty (player, player2Points)

        | Player2 -> Points (Fifteen, Fifteen)       

module Tests =
    open Xunit
    open Swensen.Unquote

    open Implementation

    [<Fact>]
    let ``should return Fifteen Love when Player1 win``() =
        test <@ play (Points (Love, Love)) Player1 = Points (Fifteen, Love) @>

    [<Fact>]
    let ``should return Fifteen Fifteen when Player2 win and previous score Fifteen Love``() =
        test <@ play (Points (Fifteen, Love)) Player2 = Points (Fifteen, Fifteen) @>

    [<Fact>]
    let ``should return Thirty Fifteen when Player1 win and previous score Fifteen Fifteen``() =
        test <@ play (Points (Fifteen, Fifteen)) Player1 = Points (Thirty, Fifteen) @>
            
    [<Fact>]
    let ``should return Fourty Fifteen when Player1 win and previous score Thirty Fifteen``() =
        test <@ play (Points (Thirty, Fifteen)) Player1 = Fourty (Player1, Fifteen) @>
