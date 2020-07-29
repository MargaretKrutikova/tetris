module Tetris

open Tetris.Types
open Tetris.Tetromino

module Constants =
  [<Literal>]
  let Width = 10
  
  [<Literal>]
  let Height = 20

[<RequireQualifiedAccess>]
type GameInput = Up | Left | Right | Down | NoOp

type Acceleration = Zero | Accelerated

type GameState = {
  Screen: Screen
  CurrentPiece: Piece
  Acceleration: Acceleration
  LastTimestampMs: float
  LinesRemoved: int
  NextPiece: Tetromino
}

module Timer =
  let getTimerMs (state: GameState) =
    match state.Acceleration with
    | Accelerated -> 60
    | Zero -> 700 

let getShapeInitialPosition (shape: Shape): Position =
  { Row = 0; Col = (Constants.Width - Shape.getWidth shape) /2 }

let newPieceFromTetromino (tetromino: Tetromino): Piece =
  let shape = tetromino |> shapeFromTetromino
  { 
    ScreenPosition = getShapeInitialPosition shape
    Shape = shape 
    Color = tetromino.Color
  }

let initGameState (currentTetromino, nextTetromino): GameState = {
  Screen = Screen.makeEmptyScreen (Screen.Width Constants.Width) (Screen.Height Constants.Height)
  CurrentPiece = currentTetromino |> newPieceFromTetromino
  LastTimestampMs = 0.0
  Acceleration = Zero
  LinesRemoved = 0
  NextPiece = nextTetromino
}

module Collision =
  let private isPositionOccuped (screen: Screen) (position: Position) =
    screen 
      |> Screen.findTileAtPosition position
      |> Option.map (fun tile -> isTileFilled tile.Type)
      |> Option.defaultValue true

  let hasCollisions (screen: Screen) (piece: Piece) =
    piece.Shape 
      |> Seq.map (Screen.getAbsolutePosition piece.ScreenPosition)
      |> Seq.exists (isPositionOccuped screen)

let updatePieceIfNoCollision (gameState: GameState) (piece: Piece) =
  if (Collision.hasCollisions gameState.Screen piece) then
    gameState
  else 
    { gameState with CurrentPiece = piece }

let processGameInputKeyDown (input: GameInput) (state: GameState) : GameState =
  match input with
  | GameInput.Left -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveLeft |> updatePieceIfNoCollision state
  | GameInput.Right -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveRight |> updatePieceIfNoCollision state
  | GameInput.Up -> 
    state.CurrentPiece |> Piece.updateShape Shape.rotateClockwise |> updatePieceIfNoCollision state
  | GameInput.Down -> { state with Acceleration = Accelerated }
  | GameInput.NoOp -> state

let processGameInputKeyUp (input: GameInput) (state: GameState) : GameState =
  match input with
    | GameInput.Down -> { state with Acceleration = Zero }
    | _ -> state

let hasPieceLanded (screen: Screen) (piece: Piece) =
  piece 
  |> Piece.updatePosition Position.moveDown 
  |> Collision.hasCollisions screen

let drawPiece (piece: Piece) (screen: Screen): Screen =
  let getPieceTileType (screenPosition: Position) = 
      piece.Shape 
        |> Seq.map (Screen.getAbsolutePosition piece.ScreenPosition)
        |> Seq.tryFind (Position.areEqual screenPosition)
        |> Option.map (fun _ -> Filled piece.Color)

  let updateScreenTileType (screenTile: Tile): Tile =
    { screenTile with Type = getPieceTileType screenTile.Position |> Option.defaultValue screenTile.Type }

  screen |> Seq.map (Seq.map updateScreenTileType >> Seq.toList) |> Seq.toList

let copyNextPieceToCurrent (randomTetromino: Tetromino) (state: GameState): GameState =
  { state with CurrentPiece = newPieceFromTetromino state.NextPiece; NextPiece = randomTetromino }

let landTetromino (state: GameState): GameState =
  let (linesRemoved, screen) = 
    state.Screen 
    |> drawPiece state.CurrentPiece 
    |> Screen.removeFilledLines (Screen.Width Constants.Width)

  { state with Screen = screen }
