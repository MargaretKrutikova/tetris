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
    let ScreenColor = "gray"


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
    GameState: Tetris.GameState
}

type Msg =
    | GameLoop
    | KeyPressed of Tetris.GameInput 

let init() : Model * Cmd<Msg>= 
    { GameState = Tetris.initGameState () }, Cmd.none // TODO: pass randomly generated shape from command

// UPDATE

let update (msg:Msg) (model: Model) =
    match msg with
    | GameLoop -> { model with GameState = Tetris.gameLoop model.GameState }, Cmd.none
    | KeyPressed input -> { model with GameState = Tetris.gameInput input model.GameState }, Cmd.none

let keyboardInputs (dispatch) =
    document.addEventListener("keydown", fun e -> 
        let event = e :?> Types.KeyboardEvent
        event.key |> keyToGameInput |> KeyPressed |> dispatch)

// VIEW 

let drawTile ({ Col = col; Row = row }: Position) (color: string) = 
    rect [ 
        SVGAttr.Stroke("black")
        SVGAttr.Fill(color)
        SVGAttr.Width(TileSizePx) 
        SVGAttr.Height(TileSizePx)
        X(col * TileSizePx)
        Y(row * TileSizePx)
      ] []

let drawPiece (piece: Piece) =
    let shape = piece.Shape
    let position = piece.Position

    svg [
          Style [ 
            Position PositionOptions.Relative
            Left (position.Col * TileSizePx)
            Top (position.Row * TileSizePx)
           ] 
        ] 
        (shape |> Seq.map (fun tile ->
            match tile.Type with
            | Filled color -> tetrisColorToCss color |> drawTile tile.Position |> Some
            | Empty -> None
            ) |> Seq.choose id |> Seq.toArray)

let drawScreen (state: Tetris.GameState) =
    svg [ Style [ 
            Position PositionOptions.Absolute;
            Height(TileSizePx * state.ScreenHeight)
            Width(TileSizePx * state.ScreenWidth)
        ] ] 
        (state.Screen |> Seq.map (fun tile ->
            match tile.Type with
            | Filled color -> tetrisColorToCss color |> drawTile tile.Position |> Some
            | Empty -> drawTile tile.Position Styles.ScreenColor |> Some
            ) |> Seq.choose id |> Seq.toArray)

let view (model: Model) dispatch =
  div [ Style [ Position PositionOptions.Relative; ] ]
      [ 
          drawScreen model.GameState
          drawPiece model.GameState.CurrentPiece
      ] 

let timer () =
    let sub dispatch =
        window.setInterval ((fun _ -> dispatch GameLoop), 300, []) |> ignore
    Cmd.ofSub sub

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
//|> Program.withSubscription timer
|> Program.withSubscription (fun _ -> [ timer () ; Cmd.ofSub keyboardInputs ] |> Cmd.batch)
|> Program.withConsoleTrace
|> Program.run
