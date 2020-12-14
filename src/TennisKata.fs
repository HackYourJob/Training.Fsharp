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
            | Points (Thirty, player2Points) -> Fourty (Player1, player2Points)
            | Fourty (Player1, _) -> Game(Player1)
            | Fourty (Player2, _) -> Deuce
            | Deuce -> Advantage(Player1)
            | Advantage(Player1) -> Game(Player1)
            | Advantage(Player2) -> Deuce

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

    [<Fact>]
    let ``should return Game when Player1 win and previous score Fourty Fifteen``() =
       test <@ play (Fourty (Player1, Fifteen)) Player1 = Game(Player1) @>

    [<Fact>]
    let ``should return Advantage when Player1 win and previous score Deuce``() =
        test <@ play Deuce Player1 = Advantage(Player1) @>

    [<Fact>]
    let ``should return Game when Player1 win and previous score Advantage player 1``() =
        test <@ play (Advantage(Player1)) Player1 = Game(Player1) @>

    [<Fact>]
    let ``should return Deuce when Player1 win and previous score Advantage player 2``() =
        test <@ play (Advantage(Player2)) Player1 = Deuce @>

    [<Fact>]
    let ``should return Deuce when Player1 win and previous score Thirty Fourty``() =
       test <@ play (Fourty(Player2,Thirty)) Player1 = Deuce @>
