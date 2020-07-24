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

let positionsEqual (posLeft: Position) (posRight: Position): bool =
  posLeft.Row = posRight.Row && posLeft.Col = posRight.Col

let movePositionDown (position: Position): Position =
  { position with Row = position.Row + 1 }
  
let movePositionRight (position: Position): Position =
  { position with Col = position.Col + 1 }

let movePositionLeft (position: Position): Position =
  { position with Col = position.Col - 1 }

let makeTile (tileType: TileType) (row: int) (col: int): Tile =
  { Type = tileType; Position = { Row = row; Col = col } }
 
let private rotatePositionClockwise (rowsCount: int) (position: Position): Position =
  { Row = position.Col; Col = rowsCount - position.Row - 1 }

let rotateClockwise (shape: Shape) =
  let rowsCount = shape |> Seq.map (fun tile -> tile.Position.Row) |> Seq.max |> (+) 1
  shape 
  |> Seq.map (fun tile -> { tile with Position = rotatePositionClockwise rowsCount tile.Position }) 
  |> Seq.toArray

