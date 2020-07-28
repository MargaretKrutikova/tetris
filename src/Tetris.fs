module Tetris

open Tetris.Types
open Tetris.Tetromino

module Constants =
  [<Literal>]
  let Width = 10
  
  [<Literal>]
  let Height = 20

  [<Literal>]
  let AccelerationFramesNumber = 3

type GameInput = Up | Left | Right | Down | NoOp

type Acceleration = None | Accelerated of framesLeft: int

type GameState = {
  ScreenWidth: Screen.Width
  ScreenHeight: Screen.Height
  Screen: Screen
  CurrentPiece: Piece
  Acceleration: Acceleration
  GameInput: GameInput
  LastTimestampMs: float
}

module Timer =
  let getTimerMs (state: GameState) =
    match state.Acceleration, state.CurrentPiece.State with
    | _, Landed -> 300
    | Accelerated _, _ -> 60
    | _, Falling -> 700 

let getShapeInitialPosition (shape: Shape): Position =
  { Row = 0; Col = (Constants.Width - Shape.getWidth shape) /2 }

let newPieceFromTetramino (tetromino: Tetromino): Piece =
  let shape = tetromino |> shapeFromTetromino
  { 
    ScreenPosition = getShapeInitialPosition shape
    Shape = shape 
    Color = tetromino.Color
    State = Falling 
  }

let generateNewPiece () =
  let tetromino = generateRandomTetromino () 
  newPieceFromTetramino tetromino

let initGameState (): GameState = {
  ScreenWidth = Screen.Width Constants.Width
  ScreenHeight = Screen.Height Constants.Height
  Screen = Screen.makeEmptyScreen (Screen.Width Constants.Width) (Screen.Height Constants.Height)
  CurrentPiece = generateNewPiece ()
  GameInput = NoOp
  LastTimestampMs = 0.0
  Acceleration = None
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

let gameInput (input: GameInput) (state: GameState) : GameState =
  match input with
  | Left -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveLeft |> updatePieceIfNoCollision state
  | Right -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveRight |> updatePieceIfNoCollision state
  | Up -> 
    state.CurrentPiece |> Piece.updateShape Shape.rotateClockwise |> updatePieceIfNoCollision state
  | Down -> 
    { state with Acceleration = Accelerated Constants.AccelerationFramesNumber }
  | NoOp -> state

let dropTetromino (state: GameState): GameState =
  let currentPiece =
    if hasPieceLanded state.Screen state.CurrentPiece then
      state.CurrentPiece |> Piece.updateState Landed
    else 
      state.CurrentPiece |> Piece.updatePosition Position.moveDown

  { state with CurrentPiece = currentPiece }

let landTetromino (state: GameState): GameState =
  let nextPiece = generateNewPiece ()
  let (linesRemoved, screen) = 
    drawPiece state.CurrentPiece state.Screen 
    |> Screen.removeFilledLines state.ScreenWidth

  { state with Screen = screen; CurrentPiece = nextPiece }
    
let updateTetromino (state: GameState): GameState =
  match state.CurrentPiece.State with
  | Falling -> dropTetromino state
  | Landed -> landTetromino state

let updateAcceleration (state: GameState): GameState =
  let acceleration =
    match state.Acceleration with
    | Accelerated frames when frames > 0 -> Accelerated (frames - 1)
    | Accelerated _ -> None
    | None -> None

  { state with Acceleration = acceleration }

let gameLoop (timestamp: float) (state: GameState): GameState =
  let currentTimeout = Timer.getTimerMs state
  let elapsed = timestamp - state.LastTimestampMs
  
  if elapsed >= (currentTimeout |> float) then 
    { state with LastTimestampMs = timestamp } 
      |> updateTetromino 
      |> updateAcceleration
  else 
    state
