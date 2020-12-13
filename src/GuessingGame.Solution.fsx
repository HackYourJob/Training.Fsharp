module Domain =
    type Bounds = {
        Lower: int
        Upper: int
    }
    type Guess = int
    type GuessState = FirstGuess | AnotherGuess

    type GameState =
        | Guess of Guess * Bounds * GuessState
        | KnownNumber of Guess
        | GameOver

    let makeAnotherGuess bounds =
        if bounds.Lower = bounds.Upper then
            KnownNumber bounds.Lower
        else
            let guess = (bounds.Lower + bounds.Upper) / 2
            Guess (guess, bounds, AnotherGuess)

    let initialGameState =
        let bounds = { Lower = 1; Upper = 100 }
        let guess = (bounds.Lower + bounds.Upper) / 2
        Guess (guess, bounds, FirstGuess)

module Update =
    open Domain

    type Msg =
        | Higher
        | NotHigher
        | PlayAgain
        | Quit

    let update msg gameState =
        match gameState, msg with
        | Guess (currentGuess, bounds, _), Higher ->
            makeAnotherGuess {bounds with Lower = currentGuess + 1}
        | Guess (currentGuess, bounds, _), NotHigher  ->
            makeAnotherGuess {bounds with Upper = currentGuess}
        | _, PlayAgain ->
            initialGameState
        | _, Quit  ->
            GameOver
        | GameOver, _ ->
            failwith "Should not receive any input when game is over"
        | _, _ ->
            failwith "Invalid combination of input and gamestate"

module View =
    open Domain
    open Update

    type Command = { keypress: string; msg: Msg }
    type UiElement =
        | Show of message: string
        | Input of Command list

    let isYourNumberBiggerThan guess =
        [
            Show (sprintf "Is your number bigger than %i? [y,n,q]" guess)

            let commands = [
                {keypress="y"; msg=Higher}
                {keypress="n"; msg=NotHigher}
                {keypress="q"; msg=Quit}
            ]
            Input commands
        ]

    let view gameState  =
        match gameState with
        | Guess (guess,bounds, guessState) ->
            let uiElements = isYourNumberBiggerThan guess
            match guessState with
            | FirstGuess ->
                let firstGuessElem =
                    let message = sprintf "Think of a number between %i and %i " bounds.Lower bounds.Upper
                    Show message
                firstGuessElem::uiElements
            | AnotherGuess ->
                uiElements
        | KnownNumber guess ->
            [
                Show (sprintf "Your number is %i" guess)
                Show (sprintf "Do you want to play again or quit? [y,q]")

                let commands = [
                    {keypress="y"; msg=PlayAgain}
                    {keypress="q"; msg=Quit}
                ]
                Input commands
            ]
        | GameOver ->
            [
                Show "Game over. Thanks for playing"
            ]

module GameLoop =
    open Update
    open View

    type Message = string
    type InputString = string
    type UserInterface = {
        Show: Message -> unit
        GetInputString: unit -> InputString
    }

    let handleCmd ui cmd  =
        match cmd with
        | Show msg ->
            ui.Show msg
            None
        | Input actions ->
            let displayAvaiableKeys actions =
                let validKeys =
                    actions
                    |> List.map (fun action -> action.keypress)
                    |> String.concat ", "
                ui.Show (sprintf "Please enter [%s]" validKeys)

            let rec loopUntilValidKey() =
                let key = ui.GetInputString()
                match actions |> List.tryFind (fun action -> action.keypress = key) with
                | Some action ->
                    action.msg
                | None ->
                    displayAvaiableKeys actions
                    loopUntilValidKey()

            loopUntilValidKey() |> Some

    let rec gameLoop io gameState =
        let cmds = view gameState
        let msg = cmds |> List.choose (handleCmd io) |> List.tryHead
        match msg with
        | Some msg ->
            let newState = update msg gameState
            gameLoop io newState
        | None ->
            ()

module Runner =
    open Domain
    open GameLoop

    let io = {
        Show = fun msg -> System.Console.WriteLine msg
        GetInputString = fun () -> System.Console.ReadLine()
    }

    let mainLoop () =
        gameLoop io initialGameState
