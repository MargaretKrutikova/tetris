module Tetris

open Tetris.Types
open Tetris.Tetromino

module Constants =
  [<Literal>]
  let Width = 10
  
  [<Literal>]
  let Height = 20

type GameInput = Up | Left | Right | Down | NoOp

type Acceleration = Zero | Accelerated

type GameStatus = Playing | GameOver

type GameState = {
  ScreenWidth: Screen.Width
  ScreenHeight: Screen.Height
  Screen: Screen
  CurrentPiece: Piece
  Acceleration: Acceleration
  GameInput: GameInput
  LastTimestampMs: float
  LinesRemoved: int
  GameStatus: GameStatus
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

let generateNewPiece () =
  let tetromino = generateRandomTetromino () 
  newPieceFromTetromino tetromino

let initGameState (): GameState = {
  ScreenWidth = Screen.Width Constants.Width
  ScreenHeight = Screen.Height Constants.Height
  Screen = Screen.makeEmptyScreen (Screen.Width Constants.Width) (Screen.Height Constants.Height)
  CurrentPiece = generateNewPiece ()
  GameInput = NoOp
  LastTimestampMs = 0.0
  Acceleration = Zero
  LinesRemoved = 0
  GameStatus = Playing
  NextPiece = generateRandomTetromino () 
}

module Effects =
  type Cmd =
    | SpawnTetromino

  type Msg = 
    | SpawnedTetromino of Tetromino
  
  let mapEffect (effectCmd: Cmd) (dispatch: Msg -> unit) =
    match effectCmd with
    | SpawnTetromino ->
      generateRandomTetromino () |> SpawnedTetromino |> dispatch

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

let keyDown (input: GameInput) (state: GameState) : GameState =
  match input with
  | Left -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveLeft |> updatePieceIfNoCollision state
  | Right -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveRight |> updatePieceIfNoCollision state
  | Up -> 
    state.CurrentPiece |> Piece.updateShape Shape.rotateClockwise |> updatePieceIfNoCollision state
  | Down -> { state with Acceleration = Accelerated }
  | NoOp -> state

let keyUp (input: GameInput) (state: GameState) : GameState =
  match input with
  | Down -> { state with Acceleration = Zero }
  | _ -> state

let updateGameStatus (state: GameState): GameState =
  let status = 
    if Collision.hasCollisions state.Screen state.CurrentPiece then
      GameOver
    else 
      Playing
  { state with GameStatus = status }

let copyNextPieceToCurrent (randomTetromino: Tetromino) (state: GameState): GameState =
  { state with CurrentPiece = newPieceFromTetromino state.NextPiece; NextPiece = randomTetromino }

let landTetromino (state: GameState): GameState =
  let (linesRemoved, screen) = 
    state.Screen 
    |> drawPiece state.CurrentPiece 
    |> Screen.removeFilledLines state.ScreenWidth

  { state with Screen = screen }
    
let updateTetromino (state: GameState): (GameState * Effects.Cmd option) =
  if hasPieceLanded state.Screen state.CurrentPiece then
    let newState = state |> landTetromino
    let cmd =
      match newState.GameStatus with
      | Playing -> Effects.SpawnTetromino |> Some
      | GameOver -> None
    (newState, cmd)
  else 
    { state with CurrentPiece = state.CurrentPiece |> Piece.updatePosition Position.moveDown }, None

let gameLoop (timestamp: float) (state: GameState): (GameState * Effects.Cmd option) =
  let currentTimeout = Timer.getTimerMs state
  let elapsed = timestamp - state.LastTimestampMs
  
  if elapsed >= (currentTimeout |> float) then 
    { state with LastTimestampMs = timestamp } |> updateTetromino 
  else 
    state, None

let processEffects (effect: Effects.Msg) (state: GameState): (GameState * Effects.Cmd option) =
  match effect with
  | Effects.SpawnedTetromino tetromino ->
    copyNextPieceToCurrent tetromino state, None
