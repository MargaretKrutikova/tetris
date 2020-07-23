module Tetromino

type private TetrominoLetter = I | O | T | S | Z | J | L

type TileType = Empty | Filled

type Tetromino = TileType [][]

let private intToTileType =
  function
  | 0 -> Empty
  | _ -> Filled

let private fromTetrominoLetter =
  function
  | I -> 
    [
      [0; 0; 0; 0];
      [1; 1; 1; 1];
      [0; 0; 0; 0];
      [0; 0; 0; 0];
    ] 
  | J -> [
      [1; 1; 0];
      [0; 1; 1];
      [0; 0; 0];
    ]
  | L -> [
      [0; 0; 1];
      [1; 1; 1];
      [0; 0; 0];
    ]
  | O -> [
      [0; 1; 1; 0];
      [0; 1; 1; 0];
      [0; 0; 0; 0];
    ]
  | S -> [
      [0; 1; 1];
      [1; 1; 0];
      [0; 0; 0];
    ]
  | T -> [
      [0; 1; 0];
      [1; 1; 1];
      [0; 0; 0];
    ]
  | Z -> [
      [1; 1; 0];
      [0; 1; 1];
      [0; 0; 0]
    ]

let private toTetromino (values: int list list): Tetromino =
    values |> Seq.map (Seq.map (intToTileType) >> Seq.toArray) |> Seq.toArray

let generateRandomTetromino (): Tetromino =
  let r = System.Random()
  match r.Next(7) with 
  | 0 -> I
  | 1 -> J
  | 2 -> L
  | 3 -> O
  | 4 -> S
  | 5 -> T
  | 6 -> Z
  | _ -> I
  |> fromTetrominoLetter |> toTetromino
