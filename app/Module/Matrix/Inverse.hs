module Module.Matrix.Inverse (
    inverse
) where

import Module.Matrix.Type
import Module.Matrix.Determinant (determinant, minorMatrix)

adjugate :: Matrix -> Matrix
adjugate = transposeMatrix . cofactorMatrix

cofactorMatrix :: Matrix -> Matrix
cofactorMatrix mat@(Matrix rows) =
  let n = length rows
  in Matrix
       [ [ cofactor i j mat
         | j <- [1..n]
         ]
       | i <- [1..n]
       ]

cofactor :: Int -> Int -> Matrix -> Double
cofactor i j m =
  (negate 1 ^ (i + j)) * determinant (minorMatrix i j m)
  
inverse :: Matrix -> Matrix
inverse mat =
  let d = determinant mat
  in if d == 0
     then error "inverse: matrix is singular (det = 0)"
     else scalarMultiply (1 / d) (adjugate mat)