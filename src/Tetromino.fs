module Tetris.Tetromino

open Types

type private InternalTetromino = int list list

let private colorFromTetromino =
  function
  | I -> Cyan
  | J -> Blue
  | L -> Orange
  | O -> Yellow
  | S -> Green
  | T -> Purple
  | Z -> Red

let private isFilled =  
  function
    | 0 -> false
    | _ -> true

let private makeTetrominoFromLetter =
  function
  | I -> 
    [
      [0; 0; 0; 0];
      [1; 1; 1; 1];
      [0; 0; 0; 0];
      [0; 0; 0; 0];
    ] 
  | J -> [
      [1; 1; 1];
      [0; 0; 1];
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

let generateRandomTetromino (): Tetromino =
  let r = System.Random()
  let letter =
    match r.Next(7) with 
    | 0 -> I
    | 1 -> J
    | 2 -> L
    | 3 -> O
    | 4 -> S
    | 5 -> T
    | 6 -> Z
    | _ -> I
  { Letter = letter; Color = colorFromTetromino letter }

let private mapTetrominoRow (rowIndex: int) (row: int list) =
  row 
  |> Seq.mapi (fun colIndex value -> 
      if isFilled value then
        { Row = rowIndex; Col = colIndex } |> Some
      else 
        None
  ) 
  |> Seq.choose id 
  |> Seq.toList

let private converTetrominoToShape (tetromino: InternalTetromino): Shape =
  tetromino 
    |> Seq.mapi mapTetrominoRow
    |> Seq.collect id 
    |> Seq.toArray

let shapeFromTetromino (tetromino: Tetromino): Shape =
    tetromino.Letter 
      |> makeTetrominoFromLetter 
      |> converTetrominoToShape 
