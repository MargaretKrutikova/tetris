module Tetris

type TileType = Empty | Filled

let isTileFilled =
  function
  | Filled -> true
  | Empty -> false

type Position = {
  Row: int
  Col: int
}

type Tile = {
  Type: TileType
  Position: Position
}

let makeTile (tileType: TileType) (row: int) (col: int): Tile =
  { Type = tileType; Position = { Row = row; Col = col } }

type Screen = Tile[][]

type Shape = Tile[]

type Piece = {
  Position: Position
  Shape: Shape
}

type GameState = {
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
  )

module Tetrimino =
  //type Space = I | O
  type Tetrimino = Tile[]

  let rotateClockwise (piece: Tetrimino) =
    let rows = piece |> Seq.map (fun tile -> tile.Position.Row) |> Seq.max |> (+) 1
    piece |> Seq.map (fun tile ->
      let pos = tile.Position
      makeTile tile.Type (pos.Col) (rows - pos.Row - 1)
    ) |> Seq.toArray

  let makeShape (): Tetrimino =
    [|
      [|1; 1; 0|];
      [|0; 1; 1|];
      [|0; 0; 0|];
    |] |> Seq.mapi (fun rowIndex row ->
      row |> Seq.mapi (fun colIndex value ->
          let tileType =
            match value with
            | 0 -> Empty
            | _ -> Filled
          makeTile tileType rowIndex colIndex
      )) |> Seq.collect id |> Seq.toArray

let initGameState (dimensions: Dimensions): GameState = {
  Screen = makeEmptyScreen(dimensions)
  CurrentPiece = { Position = { Row = 0; Col = dimensions.WidthTiles / 2 }; Shape = Tetrimino.makeShape()  }
}
  
let movePositionDown (position: Position): Position =
  { position with Row = position.Row + 1 }
  
let movePositionRight (position: Position): Position =
  { position with Col = position.Col + 1 }

let movePositionLeft (position: Position): Position =
  { position with Col = position.Col - 1 }

let rotateShape (gameState: GameState): GameState =
  let rotatedPiece = { gameState.CurrentPiece with Shape = Tetrimino.rotateClockwise gameState.CurrentPiece.Shape }
  { gameState with CurrentPiece = rotatedPiece }

let getTileAbsolutePosition (screenPosition: Position) (tile: Tile): Position =
  { Row =  tile.Position.Row + screenPosition.Row; Col = tile.Position.Col + screenPosition.Col}

let isPositionOccuped (screen: Screen) (position: Position) =
  screen 
    |> Seq.tryItem position.Row 
    |> Option.map (Seq.tryItem position.Col) 
    |> Option.flatten
    |> Option.map (fun tile -> isTileFilled tile.Type)
    |> Option.defaultValue true

let hasPieceLanded (screen: Screen) (piece: Piece) =
  piece.Shape 
    |> Seq.filter (fun value -> isTileFilled value.Type)
    |> Seq.map (getTileAbsolutePosition piece.Position >> movePositionDown)
    |> Seq.exists (isPositionOccuped screen)

let gameLoop (gameState: GameState): GameState =
  // is game over?
  // is falling?
  if hasPieceLanded gameState.Screen gameState.CurrentPiece then
    // generate new piece
    gameState
  else 
    let nextPosition = movePositionDown gameState.CurrentPiece.Position
    let currentPiece: Piece = { gameState.CurrentPiece with Position = nextPosition }
    { gameState with CurrentPiece = currentPiece }

//------------------------------------------------------

let copyScreen (screen: Screen): Screen =
  let arrays = screen |> Seq.cast<Tile[]>
  arrays |> Seq.map Array.copy |> Seq.toArray

let drawShape (shape: Shape) (position: Position) (screen: Screen): Screen =
  let copyScreen = screen |> copyScreen
  shape |> Seq.iter (fun tile ->
    match tile.Type with
      | Empty -> ignore()
      | Filled ->
        let (x, y) = (position.Row + tile.Position.Row, position.Col + tile.Position.Col)
        copyScreen.[x].[y] <- makeTile Filled x y
    
  )

  copyScreen
