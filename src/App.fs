module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

// CONSTANTS

let tileSizePx = 20
let screenWidthSteps = 20
let screenHeightSteps = 30

// MODEL

type Model = {
    GameState: Tetris.GameState
}

type Msg =
    | GameLoop
    | GameAction

let init() : Model * Cmd<Msg>= 
    { GameState = Tetris.initGameState { WidthTiles = screenWidthSteps; HeightTiles = screenWidthSteps } }, Cmd.none

// UPDATE

let update (msg:Msg) (model: Model) =
    match msg with
    | GameLoop -> { model with GameState = Tetris.gameLoop model.GameState }, Cmd.none
    | GameAction -> model, Cmd.none

// VIEW (rendered with React)

let tileView (left: int) (top: int) (tileSize: int) (color: string) = 
    let generatePoint (x, y) =
        (x |> string) + ", " + (y |> string)
    
    let points = [(left, top); (left + tileSize, top); (left + tileSize, top + tileSize); (left, top + tileSize)]
    let svgPointsAttr = points |> Seq.map generatePoint |> String.concat " "

    polygon [ SVGAttr.Stroke("black"); SVGAttr.Fill(color); Points(svgPointsAttr) ] []

let shapeView (tileSize: int) (positon: Tetris.ShapePosition) (shape: Tetris.Shape) =
    svg [
          SVGAttr.Custom ("shape-rendering", "crispEdges")
          Style [ 
            Position PositionOptions.Relative
            Left (positon.Col * tileSize)
            Top (positon.Row * tileSize)
           ] 
        ] 
        (shape |> Seq.mapi (fun rowInd row -> 
                row |> Seq.mapi (fun colInd tile ->
                    match tile with
                    | Tetris.Filled -> tileView (colInd * tileSize) (rowInd * tileSize) tileSize "yellow" |> Some
                    | Tetris.Empty -> None
                )
        ) |> Seq.concat |> Seq.choose id |> Seq.toArray)

let view (model: Model) dispatch =

  div []
      [ div [ Style [ Height "600px"; Width "500px"; BackgroundColor "orange" ] ] [
          Tetris.makeShape () |> shapeView tileSizePx { Row = 10; Col = 5 }
      ] ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
