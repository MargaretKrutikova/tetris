module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Browser
open Tetris.Types

// CONSTANTS

module Styles =
    [<Literal>]
    let ScreenColor = "lightgray"


[<Literal>]
let TileSizePx = 25

let keyToGameInput (key: string): Tetris.GameInput =
  match key with
  | "ArrowUp" -> Tetris.Up
  | "ArrowLeft" -> Tetris.Left
  | "ArrowRight" -> Tetris.Right
  | "ArrowDown" -> Tetris.Down
  | _ -> Tetris.NoOp

let tetrisColorToCss =
    function
    | Cyan -> "cyan" 
    | Blue -> "blue" 
    | Yellow -> "yellow"
    | Orange -> "orange" 
    | Green -> "lawngreen"
    | Purple -> "purple"
    | Red -> "red"

// MODEL

type Model = {
    GameStatus: Tetris.GameStatus
}

type Msg =
    | GameLoop of timestamp: float
    | GameMessage of Tetris.Effects.Msg
    | KeyDown of Tetris.GameInput 
    | KeyUp of Tetris.GameInput
    | StartGame

let init() : Model * Cmd<Msg>= 
    { GameStatus = Tetris.NotStarted }, Cmd.none 

// UPDATE

let fromGameEffect (gameEffect: Tetris.Effects.Cmd option): Msg Cmd =
    match gameEffect with
    | Some effect -> 
        Cmd.ofSub (fun (dispatch: Msg -> unit) -> 
            Tetris.Effects.mapEffect effect (GameMessage >> dispatch)
        )
    | None -> Cmd.none

let update (msg:Msg) (model: Model) =
    match msg with
    | GameLoop timestamp -> 
        let gameStatus, gameEffect = Tetris.gameLoop timestamp model.GameStatus
        { model with GameStatus = gameStatus }, fromGameEffect gameEffect

    | GameMessage gameMsg -> 
        let gameStatus, gameEffect = Tetris.processEffects gameMsg model.GameStatus
        { model with GameStatus = gameStatus }, fromGameEffect gameEffect

    | KeyDown input -> { model with GameStatus = Tetris.keyDown input model.GameStatus }, Cmd.none
    | KeyUp input -> { model with GameStatus = Tetris.keyUp input model.GameStatus }, Cmd.none
    | StartGame -> 
        let gameStatus, gameEffect = Tetris.startGame ()
        { model with GameStatus = gameStatus }, fromGameEffect gameEffect

let keyboardInputs dispatch =
    let keyFromEvent (e: Types.Event) =
        let event = e :?> Types.KeyboardEvent
        event.key 

    document.addEventListener("keydown", keyFromEvent >> keyToGameInput >> KeyDown >> dispatch)
    document.addEventListener("keyup", keyFromEvent >> keyToGameInput >> KeyUp >> dispatch)

// VIEW 

let drawTile (color: string) ({ Col = col; Row = row }: Position) = 
    rect [ 
        SVGAttr.Stroke("black")
        SVGAttr.Fill(color)
        SVGAttr.Width(TileSizePx) 
        SVGAttr.Height(TileSizePx)
        X(col * TileSizePx)
        Y(row * TileSizePx)
      ] []

let drawPiece (piece: Piece) =
    let position = piece.ScreenPosition
    let pieceColor = tetrisColorToCss piece.Color

    svg [
          Style [ 
            Position PositionOptions.Relative
            Left (position.Col * TileSizePx)
            Top (position.Row * TileSizePx)
           ] 
        ] 
        (piece.Shape |> Seq.map (drawTile pieceColor) |> Seq.toArray)

let drawScreenTile (screenTile: Tile) =
    let color = 
        match screenTile.Type with
        | Filled color -> tetrisColorToCss color 
        | Empty -> Styles.ScreenColor
    drawTile color screenTile.Position 

let drawScreen (state: Tetris.GameState) =
    svg [ Style [ 
            Position PositionOptions.Absolute;
            Height(TileSizePx * Tetris.Constants.Height)
            Width(TileSizePx * Tetris.Constants.Width)
        ] ] 
        (state.Screen |> Seq.collect (Seq.map drawScreenTile) |> Seq.toArray)

let nextTetromino (tetromino: Tetromino) =
    let shape = Tetris.Tetromino.shapeFromTetromino tetromino
    let color = tetrisColorToCss tetromino.Color

    svg [ Style []] 
      (shape |> Seq.map (drawTile color) |> Seq.toArray)

let gameView (gameState: Tetris.GameState) =
  div [ Style [ Display DisplayOptions.Flex ] ] [
    div [ Style [ Position PositionOptions.Relative; ] ]
          [ 
              drawScreen gameState
              drawPiece gameState.CurrentPiece
          ] 
    div [] [
        h3 [ Style [MarginBottom "30px"] ] [str "Next tetromino:"]
        nextTetromino gameState.NextPiece]
  ]

let view (model: Model) dispatch =
    div [] [
        button [ OnClick (fun _ -> dispatch StartGame) ] [str "Start"]
        // span [] [sprintf "%A" model |> str]
        match model.GameStatus with
        | Tetris.NotStarted -> div [] [str "Not Started"] // TODO: draw empty screen 
        | Tetris.Playing state -> gameView state
        | Tetris.GameOver state -> gameView state
    ]

let timer () =
    let rec gameLoop dispatch =
        window.requestAnimationFrame(
            fun timestamp -> 
                GameLoop timestamp |> dispatch
                gameLoop dispatch
           ) |> ignore
    Cmd.ofSub gameLoop

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription (fun _ -> [ timer () ; Cmd.ofSub keyboardInputs ] |> Cmd.batch)
|> Program.withConsoleTrace
|> Program.run
