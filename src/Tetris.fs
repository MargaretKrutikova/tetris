module Tetris

open Tetris.Types
open Tetris.Tetromino

let getRandomShape = generateRandomTetromino >> converTetrominoToShape

module Constants = 
  let widthCellsCount = 10
  let heightCellsCount = 24

type Screen = Tile[]

type Piece = {
  Position: Position
  Shape: Shape
}

type GameState = {
  ScreenWidth: int
  Screen: Screen
  CurrentPiece: Piece
}

type GameInput = Up | Left | Right | NoOp

let keyToGameInput (key: string): GameInput =
    match key with
    | "ArrowUp" -> Up
    | "ArrowLeft" -> Left
    | "ArrowRight" -> Right
    | _ -> NoOp

type Dimensions = {
  WidthTiles: int
  HeightTiles: int
}

let makeEmptyScreen (dimensions: Dimensions) : Screen =
  Array.init dimensions.HeightTiles (fun rowIndex -> 
    Array.init dimensions.WidthTiles (fun colIndex -> makeTile Empty rowIndex colIndex)
  ) |> Seq.concat |> Seq.toArray

let generateNewPiece (screenColumns: int) =
  {
    Position = { Row = 0; Col = screenColumns / 2 }
    Shape = getRandomShape() 
  }

let initGameState (dimensions: Dimensions): GameState = {
  ScreenWidth = dimensions.WidthTiles;
  Screen = makeEmptyScreen(dimensions)
  CurrentPiece = generateNewPiece dimensions.WidthTiles
}
  
let rotateShape (gameState: GameState): GameState =
  let rotatedPiece = { gameState.CurrentPiece with Shape = rotateClockwise gameState.CurrentPiece.Shape }
  { gameState with CurrentPiece = rotatedPiece }

let getTileAbsolutePosition (screenPosition: Position) (tile: Tile): Position =
  { Row =  tile.Position.Row + screenPosition.Row; Col = tile.Position.Col + screenPosition.Col}

let isPositionOccuped (screen: Screen) (position: Position) =
  screen 
    |> Seq.tryFind (fun tile -> positionsEqual tile.Position position)
    |> Option.map (fun tile -> isTileFilled tile.Type)
    |> Option.defaultValue true

let hasPieceLanded (screen: Screen) (piece: Piece) =
  piece.Shape 
    |> Seq.filter (fun value -> isTileFilled value.Type)
    |> Seq.map (getTileAbsolutePosition piece.Position >> movePositionDown)
    |> Seq.exists (isPositionOccuped screen)

let drawPiece (piece: Piece) (screen: Screen): Screen =
  let tilesToDraw = 
    piece.Shape 
    |> Seq.filter (fun tile -> isTileFilled tile.Type)
    |> Seq.map (getTileAbsolutePosition piece.Position)
  
  let shouldFill (position) =
    tilesToDraw |> Seq.exists (positionsEqual position)

  screen 
    |> Seq.map (fun tile -> if shouldFill tile.Position then { tile with Type = Filled } else tile) 
    |> Seq.toArray

let gameLoop (gameState: GameState): GameState =
  // is game over?
  // is falling?
  if hasPieceLanded gameState.Screen gameState.CurrentPiece then
    let nextPiece = generateNewPiece gameState.ScreenWidth
    let screen = drawPiece gameState.CurrentPiece gameState.Screen
    { gameState with Screen = screen; CurrentPiece = nextPiece }
  else 
    let nextPosition = movePositionDown gameState.CurrentPiece.Position
    let currentPiece: Piece = { gameState.CurrentPiece with Position = nextPosition }
    { gameState with CurrentPiece = currentPiece }


let gameInput (input: GameInput) (gameState: GameState) : GameState =
  match input with
  | Left -> 
    let piece = { gameState.CurrentPiece with Position = movePositionLeft gameState.CurrentPiece.Position }
    { gameState with CurrentPiece = piece }
  | Right -> 
    let piece = { gameState.CurrentPiece with Position = movePositionRight gameState.CurrentPiece.Position }
    { gameState with CurrentPiece = piece }
  | Up -> 
    let piece = { gameState.CurrentPiece with Shape = rotateClockwise gameState.CurrentPiece.Shape }
    { gameState with CurrentPiece = piece }
  | NoOp -> 
    gameState

   