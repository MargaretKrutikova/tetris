module Tetris

type Tile = Empty | Filled

type Screen = Tile[][]

type Shape = Tile[][]

type ShapePosition = {
  Row: int
  Col: int
}

type GameState = {
  Screen: Screen
  CurrentShape: Shape
  ShapePosition: ShapePosition
}

type GameAction = MoveRight | MoveLeft | Rotate | DoNothing

type Dimensions = {
  WidthTiles: int
  HeightTiles: int
}

let makeEmptyScreen (dimensions: Dimensions) : Screen =
  Array.init dimensions.HeightTiles (fun _ -> 
    Array.init dimensions.WidthTiles (fun _ -> Empty)
  )
  
let makeShape (): Shape =
  [|
    [|Filled;Filled|]
    [|Filled|]
    [|Filled|]
  |]
  

let initGameState (dimensions: Dimensions): GameState = {
  Screen = makeEmptyScreen(dimensions)
  CurrentShape = makeShape()
  ShapePosition = { Row = 0; Col = dimensions.WidthTiles / 2 }
}
  
let moveShapeDown (position: ShapePosition): ShapePosition =
  { position with Row = position.Row + 1 }
  
let moveShapeRight (position: ShapePosition): ShapePosition =
  { position with Col = position.Col + 1 }

let moveShapeLeft (position: ShapePosition): ShapePosition =
  { position with Col = position.Col - 1 }

let gameLoop (gameState: GameState): GameState =
  let nextShapePosition = moveShapeDown gameState.ShapePosition
  { gameState with ShapePosition = nextShapePosition }

//------------------------------------------------------

let copyScreen (screen: Screen): Screen =
  let arrays = screen |> Seq.cast<Tile[]>
  arrays |> Seq.map Array.copy |> Seq.toArray

let drawShape (shape: Shape) (position: ShapePosition) (screen: Screen): Screen =
  let copyScreen = screen |> copyScreen
  shape |> Array.iteri (fun rowInd row -> 
    Seq.iteri (fun colInd tile -> 
      match tile with
      | Empty -> ignore()
      | Filled ->
        copyScreen.[position.Row + rowInd].[position.Col + colInd] <- Filled
    ) row
  )
  copyScreen
