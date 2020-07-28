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

type GameState = {
  Screen: Screen
  CurrentPiece: Piece
  Acceleration: Acceleration
  LastTimestampMs: float
  LinesRemoved: int
  NextPiece: Tetromino
}

type GameStatus = 
  | NotStarted 
  | Playing of GameState
  | GameOver of GameState

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

module Effects =
  type Cmd =
    | SpawnTetromino
    | InitGame

  type Msg = 
    | SpawnedTetromino of Tetromino
    | InitGameDone of pair: (Tetromino * Tetromino)
  
  let mapEffect (effectCmd: Cmd) (dispatch: Msg -> unit) =
    match effectCmd with
    | SpawnTetromino ->
      generateRandomTetromino () |> SpawnedTetromino |> dispatch
    | InitGame ->
      (generateRandomTetromino (), generateRandomTetromino ()) |> InitGameDone |> dispatch

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

let private processKeyDown (input: GameInput) (state: GameState) : GameState =
  match input with
  | Left -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveLeft |> updatePieceIfNoCollision state
  | Right -> 
    state.CurrentPiece |> Piece.updatePosition Position.moveRight |> updatePieceIfNoCollision state
  | Up -> 
    state.CurrentPiece |> Piece.updateShape Shape.rotateClockwise |> updatePieceIfNoCollision state
  | Down -> { state with Acceleration = Accelerated }
  | NoOp -> state

let keyDown (input: GameInput) (status: GameStatus) : GameStatus =
  match status with
  | Playing state -> processKeyDown input state |> Playing
  | _ -> status

let keyUp (input: GameInput) (status: GameStatus) : GameStatus =
  match status with
  | Playing state ->
    match input with
    | Down -> { state with Acceleration = Zero }
    | _ -> state
    |> Playing
  | _ -> status

let private copyNextPieceToCurrent (randomTetromino: Tetromino) (state: GameState): GameState =
  { state with CurrentPiece = newPieceFromTetromino state.NextPiece; NextPiece = randomTetromino }

let private landTetromino (state: GameState): GameState =
  let (linesRemoved, screen) = 
    state.Screen 
    |> drawPiece state.CurrentPiece 
    |> Screen.removeFilledLines (Screen.Width Constants.Width)

  { state with Screen = screen }
    
let private updateTetromino (state: GameState): (GameState * Effects.Cmd option) =
  if hasPieceLanded state.Screen state.CurrentPiece then
    (state |> landTetromino, Effects.SpawnTetromino |> Some)
  else 
    { state with CurrentPiece = state.CurrentPiece |> Piece.updatePosition Position.moveDown }, None

let private updateGameState (timestamp: float) (state: GameState): (GameState * Effects.Cmd option) =
  let currentTimeout = Timer.getTimerMs state
  let elapsed = timestamp - state.LastTimestampMs
  
  if elapsed >= (currentTimeout |> float) then 
    { state with LastTimestampMs = timestamp } |> updateTetromino 
  else 
    state, None

let gameLoop (timestamp: float) (status: GameStatus): (GameStatus * Effects.Cmd option) =
  match status with
  | Playing state ->
    let gameState, effect = updateGameState timestamp state
    Playing gameState, effect
  | _ -> status, None

let startGame () =
  NotStarted, Effects.InitGame |> Some

let checkEndGame (gameState: GameState) =
  if Collision.hasCollisions gameState.Screen gameState.CurrentPiece then
      GameOver gameState
  else 
    Playing gameState

let processEffects (effect: Effects.Msg) (status: GameStatus): (GameStatus * Effects.Cmd option) =
  match status, effect with
  | Playing state, Effects.SpawnedTetromino tetromino ->
    copyNextPieceToCurrent tetromino state |> checkEndGame, None
  | NotStarted, Effects.InitGameDone pair -> 
    initGameState pair |> Playing, None
  | _ -> status, None
