module Tetris.Types

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

type Shape = Tile[]

type Piece = {
  Position: Position
  Shape: Shape
}

type Screen = Tile[]

module Position =
  let areEqual (posLeft: Position) (posRight: Position): bool =
    posLeft.Row = posRight.Row && posLeft.Col = posRight.Col

  let moveDown (position: Position): Position =
    { position with Row = position.Row + 1 }
    
  let moveRight (position: Position): Position =
    { position with Col = position.Col + 1 }

  let moveLeft (position: Position): Position =
    { position with Col = position.Col - 1 }

module Tile =
  let make (tileType: TileType) (row: int) (col: int): Tile =
    { Type = tileType; Position = { Row = row; Col = col } }

module Shape =
  let private rotatePositionClockwise (rowsCount: int) (position: Position): Position =
    { Row = position.Col; Col = rowsCount - position.Row - 1 }

  let rotateClockwise (shape: Shape) =
    let rowsCount = shape |> Seq.map (fun tile -> tile.Position.Row) |> Seq.max |> (+) 1
    shape 
    |> Seq.map (fun tile -> { tile with Position = rotatePositionClockwise rowsCount tile.Position }) 
    |> Seq.toArray

  let getWidth (shape: Shape) =
    shape |> Seq.map (fun tile -> tile.Position.Col) |> Seq.max |> (+) 1

module Piece =
  let updatePosition (positionFn: Position -> Position) (piece: Piece): Piece =
    { piece with Position = positionFn piece.Position }

  let updateShape (shapeFn: Shape -> Shape) (piece: Piece): Piece =
    { piece with Shape = shapeFn piece.Shape }

module Screen =
  type Width = Width of int
  type Height = Height of int

  let getTileAbsolutePosition (screenPosition: Position) (tile: Tile): Position =
      { Row = tile.Position.Row + screenPosition.Row; Col = tile.Position.Col + screenPosition.Col}

  let makeEmptyScreen (Width width) (Height height) : Screen =
    Array.init height (fun rowIndex -> 
      Array.init width (fun colIndex -> Tile.make Empty rowIndex colIndex)
    ) 
    |> Seq.concat 
    |> Seq.toArray
