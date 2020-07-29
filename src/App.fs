module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Browser
open Tetris.Types
open Tetris

// CONSTANTS

module Styles =
    [<Literal>]
    let ScreenColor = "lightgray"


[<Literal>]
let TileSizePx = 25

let keyToGameInput (key: string): Tetris.GameInput =
  match key with
  | "ArrowUp" -> GameInput.Up
  | "ArrowLeft" -> GameInput.Left
  | "ArrowRight" -> GameInput.Right
  | "ArrowDown" -> GameInput.Down
  | _ -> GameInput.NoOp

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

type GameStatus = 
  | NotStarted 
  | Playing of GameState
  | GameOver of GameState

type Model = {
    GameStatus: GameStatus
}

type Msg =
    | GameLoop of timestamp: float
    | KeyDown of Tetris.GameInput 
    | KeyUp of Tetris.GameInput
    | StartGame
    | SpawnedTetromino of Tetromino
    | InitGameDone of pair: (Tetromino * Tetromino)

let init() : Model * Cmd<Msg>= 
    { GameStatus = NotStarted }, Cmd.none 

// UPDATE


let keyDown (input: GameInput) (status: GameStatus) : GameStatus =
  match status with
  | Playing state -> processGameInputKeyDown input state |> Playing
  | _ -> status

let keyUp (input: GameInput) (status: GameStatus) : GameStatus =
  match status with
  | Playing state -> processGameInputKeyUp input state |> Playing
  | _ -> status
    
let spawnTetromino dispatch =
    Tetromino.generateRandomTetromino () |> SpawnedTetromino |> dispatch

let generateStartGameTetrominos dispatch = 
    (Tetromino.generateRandomTetromino (), Tetromino.generateRandomTetromino ()) 
        |> InitGameDone 
        |> dispatch

let updateTetromino (state: GameState): GameState * Msg Cmd =
  if hasPieceLanded state.Screen state.CurrentPiece then
    state |> landTetromino, Cmd.ofSub spawnTetromino 
  else 
    { state with CurrentPiece = state.CurrentPiece |> Piece.updatePosition Position.moveDown }, Cmd.none

let updateGameState (timestamp: float) (state: GameState): GameState * Msg Cmd =
  let currentTimeout = Timer.getTimerMs state
  let elapsed = timestamp - state.LastTimestampMs
  
  if elapsed >= (currentTimeout |> float) then 
    { state with LastTimestampMs = timestamp } |> updateTetromino 
  else 
    state, Cmd.none

let checkEndGame (gameState: GameState) =
  if Collision.hasCollisions gameState.Screen gameState.CurrentPiece then
    GameOver gameState
  else 
    Playing gameState

let update (msg:Msg) (model: Model) =
    match msg, model.GameStatus with
    | GameLoop timestamp, Playing state -> 
        let gameState, cmd = updateGameState timestamp state
        { model with GameStatus = Playing gameState }, cmd

    | KeyDown input, Playing _ -> 
        { model with GameStatus = keyDown input model.GameStatus }, Cmd.none

    | KeyUp input, Playing _ -> 
        { model with GameStatus = keyUp input model.GameStatus }, Cmd.none

    | StartGame, _ -> 
        { model with GameStatus = NotStarted }, Cmd.ofSub generateStartGameTetrominos

    | SpawnedTetromino tetromino, Playing state ->
        let gameState = copyNextPieceToCurrent tetromino state |> checkEndGame
        { model with GameStatus = gameState }, Cmd.none
        
    | InitGameDone pair, _ -> 
        { model with GameStatus = initGameState pair |> Playing }, Cmd.none

    | GameLoop _, _ -> model, Cmd.none
    | KeyDown _, _ -> model, Cmd.none
    | KeyUp _, _ -> model, Cmd.none
    | SpawnedTetromino _, _ -> model, Cmd.none


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
        | NotStarted -> div [] [str "Not Started"] // TODO: draw empty screen 
        | Playing state -> gameView state
        | GameOver state -> gameView state
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
