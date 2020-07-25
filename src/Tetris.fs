module Tetris

open Tetris.Types
open Tetris.Tetromino

module Constants =
  [<Literal>]
  let Width = 10
  
  [<Literal>]
  let Height = 20

type GameState = {
  ScreenWidth: Screen.Width
  ScreenHeight: Screen.Height
  Screen: Screen
  CurrentPiece: Piece
}

module Timer =
  let getTimerMs (state: GameState) =
    match state.CurrentPiece.State with
    | Falling -> 400
    | Dropped -> 50
    | Landed -> 100

let getShapeInitialPosition (shape: Shape): Position =
  { Row = 0; Col = (Constants.Width - Shape.getWidth shape) /2 }

let generateNewPiece () =
  let tetromino = generateRandomTetromino () 
  let shape = tetromino |> shapeFromTetromino
  { Position = getShapeInitialPosition shape; Shape = shape; State = Falling }

let initGameState (): GameState = {
  ScreenWidth = Screen.Width Constants.Width
  ScreenHeight = Screen.Height Constants.Height
  Screen = Screen.makeEmptyScreen (Screen.Width Constants.Width) (Screen.Height Constants.Height)
  CurrentPiece = generateNewPiece ()
}

type GameInput = Up | Left | Right | Down | NoOp

module Collision =
  let private isPositionOccuped (screen: Screen) (position: Position) =
    screen 
      |> Seq.tryFind (fun tile -> Position.areEqual tile.Position position)
      |> Option.map (fun tile -> isTileFilled tile.Type)
      |> Option.defaultValue true

  let hasCollisions (screen: Screen) (piece: Piece) =
    piece.Shape 
      |> Seq.filter (fun value -> isTileFilled value.Type)
      |> Seq.map (Screen.getTileAbsolutePosition piece.Position)
      |> Seq.exists (isPositionOccuped screen)

let updatePieceIfNoCollision (gameState: GameState) (piece: Piece) =
  if (Collision.hasCollisions gameState.Screen piece) then
    gameState
  else 
    { gameState with CurrentPiece = piece }

let hasPieceLanded (screen: Screen) (piece: Piece) =
  piece |> Piece.updatePosition Position.moveDown |> Collision.hasCollisions screen

let drawPiece (piece: Piece) (screen: Screen): Screen =
  let findTileToFill (position) = 
      piece.Shape |> Seq.tryFind (
          fun tile -> tile |> Screen.getTileAbsolutePosition piece.Position |> Position.areEqual position)
      |> Option.map (fun tile -> tile.Type)

  screen 
    |> Seq.map (fun tile -> 
       match findTileToFill tile.Position with
       | Some (Filled color) -> { tile with Type = Filled color }
       | Some Empty | None -> tile
       ) 
    |> Seq.toArray

let gameInput (input: GameInput) (state: GameState) : GameState =
  match input with
  | Left -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveLeft |> updatePieceIfNoCollision state
  | Right -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveRight |> updatePieceIfNoCollision state
  | Up -> 
    state.CurrentPiece |> Piece.updateShape Shape.rotateClockwise |> updatePieceIfNoCollision state
  | Down -> 
    { state with CurrentPiece = state.CurrentPiece |> Piece.updateState Dropped }
  | NoOp -> state

let isLineFilled (line: Tile seq): bool =
  line |> Seq.forall (fun tile -> isTileFilled tile.Type)

let initEmptyLine (Screen.Width screenWidth) (row: int) : Tile seq =
  Seq.init screenWidth (fun ind -> Tile.make Empty row ind)

let removeFilledRows (Screen.Height height) (width: Screen.Width) (screen: Screen): Screen =
  let notFilledLines =
    screen 
    |> Seq.groupBy (fun tile -> tile.Position.Row)
    |> Seq.filter (fun (_, line) -> isLineFilled line |> not)
    |> Seq.map snd
    |> Seq.toList
  
  let linesRemoved = height - (notFilledLines |> Seq.length)
  let emptyLinesToAdd = Seq.init linesRemoved (initEmptyLine width) |> Seq.toList

  if linesRemoved > 0 then
    (emptyLinesToAdd @ notFilledLines)
      |> Seq.mapi (fun rowIndex row -> 
          row |> Seq.map (fun tile -> { tile with Position = { tile.Position with Row = rowIndex } })
        )
     |> Seq.concat
     |> Seq.toArray
  else 
    screen

let gameLoop (state: GameState): GameState =
  // is game over?
  // has rows to remove
  match state.CurrentPiece.State with
  | Falling 
  | Dropped ->
    let currentPiece =
      if hasPieceLanded state.Screen state.CurrentPiece then
        state.CurrentPiece |> Piece.updateState Landed
      else 
      state.CurrentPiece |> Piece.updatePosition Position.moveDown

    { state with CurrentPiece = currentPiece }

  | Landed ->
    let nextPiece = generateNewPiece ()
    let screen = 
      drawPiece state.CurrentPiece state.Screen 
      |> removeFilledRows state.ScreenHeight state.ScreenWidth

    { state with Screen = screen; CurrentPiece = nextPiece }
    