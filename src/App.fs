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
let screenWidthSteps = 15
let screenHeightSteps = 20

// MODEL

type Model = {
    GameState: Tetris.GameState
}

type Msg =
    | GameLoop
    | GameAction
    | Rotate

let init() : Model * Cmd<Msg>= 
    { GameState = Tetris.initGameState { WidthTiles = screenWidthSteps; HeightTiles = screenHeightSteps } }, Cmd.none

// UPDATE

let update (msg:Msg) (model: Model) =
    match msg with
    | GameLoop -> { model with GameState = Tetris.gameLoop model.GameState }, Cmd.none
    | GameAction -> model, Cmd.none
    | Rotate -> { model with GameState = Tetris.rotateShape model.GameState }, Cmd.none

// VIEW (rendered with React)

let drawTile ({ Col = col; Row = row }: Tetris.Position) (tileSize: int) (color: string) = 
    let generatePoint (x, y) =
        (x |> string) + ", " + (y |> string)
    
    let left = col * tileSize
    let top = row * tileSize

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
            | Tetromino.Filled -> drawTile tile.Position tileSize "yellow" |> Some
            | Tetromino.Empty -> None
            ) |> Seq.choose id |> Seq.toArray)

let drawScreen (tileSize: int) (screen: Tetris.Screen) =
    svg [ Style [ 
            Position PositionOptions.Absolute
            Height (tileSize * screenHeightSteps)
            Width (tileSize * screenWidthSteps)
            ] ] 
        (screen |> Seq.map (fun tile ->
            match tile.Type with
            | Tetromino.Filled -> drawTile tile.Position tileSize "yellow" |> Some
            | Tetromino.Empty -> drawTile tile.Position tileSize "gray" |> Some
            ) |> Seq.choose id |> Seq.toArray)

let view (model: Model) dispatch =

  div [ Style [ Position PositionOptions.Relative ] ]
      [ 
          button [ OnClick (fun _ -> dispatch Rotate) ] [str "Rotate"]
          drawScreen tileSizePx model.GameState.Screen
          drawPiece tileSizePx model.GameState.CurrentPiece
      ] 

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
