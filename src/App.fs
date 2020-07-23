module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Browser

// CONSTANTS

let tileSizePx = 30
let screenWidthSteps = 20
let screenHeightSteps = 30

// MODEL

type Model = {
    GameState: Tetris.GameState
}

type Msg =
    | GameLoop
    | GameAction
    | Rotate

let init() : Model * Cmd<Msg>= 
    { GameState = Tetris.initGameState { WidthTiles = screenWidthSteps; HeightTiles = screenWidthSteps } }, Cmd.none

// UPDATE

let update (msg:Msg) (model: Model) =
    match msg with
    | GameLoop -> { model with GameState = Tetris.gameLoop model.GameState }, Cmd.none
    | GameAction -> model, Cmd.none
    | Rotate -> { model with GameState = Tetris.rotateShape model.GameState }, Cmd.none

// VIEW (rendered with React)

let tileView (left: int) (top: int) (tileSize: int) (color: string) = 
    let generatePoint (x, y) =
        (x |> string) + ", " + (y |> string)
    
    let points = [(left, top); (left + tileSize, top); (left + tileSize, top + tileSize); (left, top + tileSize)]
    let svgPointsAttr = points |> Seq.map generatePoint |> String.concat " "

    polygon [ SVGAttr.Stroke("black"); SVGAttr.Fill(color); Points(svgPointsAttr) ] []

let drawPiece (tileSize: int) (piece: Tetris.Piece) =
    let shape = piece.Shape
    let position = piece.Position

    svg [
          Style [ 
            Position PositionOptions.Relative
            Left (position.Col * tileSize)
            Top (position.Row * tileSize)
           ] 
        ] 
        (shape |> Seq.map (fun tile ->
            match tile.Type with
            | Tetromino.Filled -> tileView (tile.Position.Col * tileSize) (tile.Position.Row * tileSize) tileSize "yellow" |> Some
            | Tetromino.Empty -> tileView (tile.Position.Col * tileSize) (tile.Position.Row * tileSize) tileSize "gray" |> Some
            ) |> Seq.choose id |> Seq.toArray)

let view (model: Model) dispatch =

  div []
      [ 
          button [ OnClick (fun _ -> dispatch Rotate) ] [str "Rotate"]
          div [ Style [ Height "600px"; Width "500px"; BackgroundColor "orange" ] ] [
          drawPiece tileSizePx model.GameState.CurrentPiece
      ] ]

let timer initial =
    let sub dispatch =
        window.setInterval ((fun _ -> dispatch GameLoop), 300, []) |> ignore
    Cmd.ofSub sub

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
 |> Program.withSubscription timer
|> Program.withConsoleTrace
|> Program.run
