module Tetris

open Tetromino

let isTileFilled =
  function
  | Filled -> true
  | Empty -> false

type Position = {
  Row: int
  Col: int
}

let positionsEqual (posLeft: Position) (posRight: Position): bool =
  posLeft.Row = posRight.Row && posLeft.Col = posRight.Col

type Tile = {
  Type: TileType
  Position: Position
}

let makeTile (tileType: TileType) (row: int) (col: int): Tile =
  { Type = tileType; Position = { Row = row; Col = col } }

type Screen = Tile[]

module Shape =
  type Shape = Tile[]
   
  let rotateClockwise (shape: Shape) =
    let rows = shape |> Seq.map (fun tile -> tile.Position.Row) |> Seq.max |> (+) 1
    shape |> Seq.map (fun tile ->
      let pos = tile.Position
      makeTile tile.Type (pos.Col) (rows - pos.Row - 1)
    ) |> Seq.toArray

  let convertToShape (tetromino: Tetromino): Shape =
    tetromino |> Seq.mapi (fun rowIndex row ->
      row |> Seq.mapi (fun colIndex value -> makeTile value rowIndex colIndex)) 
    |> Seq.collect id |> Seq.toArray

let getRandomShape = generateRandomTetromino >> Shape.convertToShape

type Piece = {
  Position: Position
  Shape: Shape.Shape
}

type GameState = {
  ScreenWidth: int
  Screen: Screen
  CurrentPiece: Piece
}

type GameAction = MoveRight | MoveLeft | Rotate | DoNothing

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
  
let movePositionDown (position: Position): Position =
  { position with Row = position.Row + 1 }
  
let movePositionRight (position: Position): Position =
  { position with Col = position.Col + 1 }

let movePositionLeft (position: Position): Position =
  { position with Col = position.Col - 1 }

let rotateShape (gameState: GameState): GameState =
  let rotatedPiece = { gameState.CurrentPiece with Shape = Shape.rotateClockwise gameState.CurrentPiece.Shape }
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

