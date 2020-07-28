module Tetris.Types

type Color = Cyan | Blue | Yellow | Orange | Green | Purple | Red

type TileType = Empty | Filled of Color

type TetrominoLetter = I | O | T | S | Z | J | L

type Tetromino = {
  Letter: TetrominoLetter
  Color: Color
}

type Position = {
  Row: int
  Col: int
}

type Shape = Position[]

type PieceState = Falling | Dropped of framesLeft: int | Landed

type Piece = {
  State: PieceState
  ScreenPosition: Position
  Shape: Shape
  Color: Color
}

let isTileFilled =
  function
  | Filled _ -> true
  | Empty -> false

type Tile = {
  Type: TileType
  Position: Position
}

type Screen = Tile list list

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
    let rowsCount = shape |> Seq.map (fun tile -> tile.Row) |> Seq.max |> (+) 1
    shape 
    |> Seq.map (rotatePositionClockwise rowsCount) 
    |> Seq.toArray

  let getWidth (shape: Shape) =
    shape |> Seq.map (fun tile -> tile.Col) |> Seq.max |> (+) 1

module Piece =
  let updatePosition (positionFn: Position -> Position) (piece: Piece): Piece =
    { piece with ScreenPosition = positionFn piece.ScreenPosition }

  let updateShape (shapeFn: Shape -> Shape) (piece: Piece): Piece =
    { piece with Shape = shapeFn piece.Shape }

  let updateState (state: PieceState) (piece: Piece): Piece =
    { piece with State = state } // TODO: state machine

module Screen =
  type Width = Width of int
  type Height = Height of int

  let getAbsolutePosition (screenPosition: Position) (relativePosition: Position): Position =
      { Row = relativePosition.Row + screenPosition.Row; Col = relativePosition.Col + screenPosition.Col}

  let findTileAtPosition (position: Position) (screen: Screen): Tile option =
    screen |> Seq.concat |> Seq.tryFind (fun tile -> Position.areEqual tile.Position position)

  let makeEmptyScreen (Width width) (Height height) : Screen =
    Array.init height (fun rowIndex -> 
      Array.init width (fun colIndex -> Tile.make Empty rowIndex colIndex) |> Seq.toList
    ) 
    |> Seq.toList

  let private isLineFilled (line: Tile seq): bool =
    line |> Seq.forall (fun tile -> isTileFilled tile.Type)

  let private initEmptyLine (Width screenWidth) (row: int) : Tile list =
    Seq.init screenWidth (fun ind -> Tile.make Empty row ind) |> Seq.toList

  let private shiftLineDown (line: Tile list): Tile list =
    line |> Seq.map (fun tile -> { tile with Position = Position.moveDown tile.Position }) |> Seq.toList

  let (|Filled|NotFilled|) (line: Tile seq) =
    if isLineFilled line then
      Filled
    else 
      NotFilled

  let rec private removeFilledLinesInternal (screenWidth: Width) (linesRemoved, screen): int * Screen =
    let (found, shiftedLines) =
      Seq.foldBack (fun currentLine (hasFilledLine, lines) -> 
        if hasFilledLine then
          (hasFilledLine, (shiftLineDown currentLine) :: lines)
        else
          match currentLine with
          | Filled -> (true, lines)
          | NotFilled -> (false, currentLine :: lines)
      ) screen (false, List.empty) 

    if found then 
      (linesRemoved + 1, (initEmptyLine screenWidth 0) :: shiftedLines)
    else 
      (linesRemoved, shiftedLines)

  let removeFilledLines (screenWidth: Width) (screen: Screen): int * Screen =
    removeFilledLinesInternal screenWidth (0, screen)

