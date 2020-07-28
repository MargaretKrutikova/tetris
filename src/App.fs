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
    GameState: Tetris.GameState
}

type Msg =
    | GameLoop of timestamp: float
    | KeyDown of Tetris.GameInput 
    | KeyUp of Tetris.GameInput

let init() : Model * Cmd<Msg>= 
    { GameState = Tetris.initGameState () }, Cmd.none // TODO: pass randomly generated shape from command

// UPDATE

let update (msg:Msg) (model: Model) =
    match msg with
    | GameLoop timestamp -> 
        { model with GameState = Tetris.gameLoop timestamp model.GameState }, Cmd.none
    | KeyDown input -> { model with GameState = Tetris.keyDown input model.GameState }, Cmd.none
    | KeyUp input -> { model with GameState = Tetris.keyUp input model.GameState }, Cmd.none

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
    let (Screen.Height height, Screen.Width width) = (state.ScreenHeight, state.ScreenWidth) 
    svg [ Style [ 
            Position PositionOptions.Absolute;
            Height(TileSizePx * height)
            Width(TileSizePx * width)
        ] ] 
        (state.Screen |> Seq.collect (Seq.map drawScreenTile) |> Seq.toArray)

let view (model: Model) dispatch =
  div [ Style [ Position PositionOptions.Relative; ] ]
      [ 
          drawScreen model.GameState
          drawPiece model.GameState.CurrentPiece
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
