module Module.Matrix.Determinant where

import Module.Matrix.Type

determinant :: Matrix -> Double
determinant (Matrix m)
  | not (isSquare m) = error "determinant: matrix must be square"
  | null m           = 0
  | n == 1           = head (head m)
  | n == 2           = det2x2 m
  | otherwise        = laplaceExpand m
  where
    n = length m

det2x2 :: [[Double]] -> Double
det2x2 [[a,b],[c,d]] = a * d - b * c
det2x2 _             = error "det2x2: Not a 2×2 matrix"

laplaceExpand :: [[Double]] -> Double
laplaceExpand m =
  let row0 = head m
      n    = length row0
  in  sum
       [ (negate 1 ^ j) * (row0 !! j)
         * determinant (minorMatrix 1 (j + 1) (Matrix m))
       | j <- [0 .. n - 1]
       ]

minorMatrix :: Int -> Int -> Matrix -> Matrix
minorMatrix i j (Matrix m) =
  Matrix
    [ [ row !! c
      | c <- [0 .. length row - 1]
      , c /= (j - 1)
      ]
    | (rowIndex, row) <- zip [1..] m
    , rowIndex /= i
    ]