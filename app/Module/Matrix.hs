module Module.Matrix (
    Matrix,
    fromList,
    toList,
    determinant
) where

newtype Matrix = Matrix [[Double]]
    deriving (Show, Eq)

instance Num Matrix where
    (+) (Matrix a) (Matrix b)
        | isValid a && isValid b && sameSize a b = Matrix (zipWith (zipWith (+)) a b)
        | otherwise = error "Matrix addition: Matrices must have the same dimensions and be valid"

    (-) (Matrix a) (Matrix b)
        | isValid a && isValid b && sameSize a b = Matrix (zipWith (zipWith (-)) a b)
        | otherwise = error "Matrix subtraction: Matrices must have the same dimensions and be valid"

    (*) (Matrix a) (Matrix b)
        | isScalarMatrix b = Matrix (map (map (* scalarValue b)) a)
        | otherwise = error "Matrix multiplication: Only scalar multiplication is supported"
      where
        isScalarMatrix ([[_]]) = True
        isScalarMatrix _              = False
        scalarValue ([[x]]) = x
        scalarValue _              = error "Invalid scalar matrix"

    negate (Matrix a) = Matrix (map (map negate) a)

    abs (Matrix a) = Matrix (map (map abs) a)

    signum (Matrix a) = Matrix (map (map signum) a)

    fromInteger x = Matrix [[fromInteger x]]

fromList :: [[Double]] -> Maybe Matrix
fromList xs
    | isValid xs = Just (Matrix xs)
    | otherwise = Nothing

toList :: Matrix -> [[Double]]
toList (Matrix xs) = xs

isValid :: [[Double]] -> Bool
isValid m = allEqual (map length m)
  where
    allEqual []     = True
    allEqual (x:xs) = all (== x) xs

sameSize :: [[Double]] -> [[Double]] -> Bool
sameSize a b = (length a == length b) && (length (head a) == length (head b))

isSquare :: [[Double]] -> Bool
isSquare [] = True
isSquare m  =
  let rowCount    = length m
      colCount    = length (head m)
  in  rowCount == colCount 
      && all (\row -> length row == colCount) m

determinant :: Matrix -> Double
determinant (Matrix m)
  | not (isSquare m) = error "determinant: Matrix must be square"
  | null m           = 1
  | n == 1           = head (head m)
  | n == 2           = det2x2 m
  | otherwise        = laplaceExpand m
  where
    n = length m

laplaceExpand :: [[Double]] -> Double
laplaceExpand m =
  let row0 = head m
      n    = length row0
  in  sum [ ((-1)^(j)) * row0 !! j
            * determinant (minorMatrix 1 (j + 1) (Matrix m))
          | j <- [0..n-1] ]

det2x2 :: [[Double]] -> Double
det2x2 [[a, b], [c, d]] = a * d - b * c
det2x2 _ = error "det2x2: Not a 2×2 matrix"

minorMatrix :: Int -> Int -> Matrix -> Matrix
minorMatrix i j (Matrix m) =
  Matrix
    [ [ row !! col
      | col <- [0 .. length row - 1]
      , col /= (j - 1)
      ]
    | (rowIndex, row) <- zip [1..] m
    , rowIndex /= i
    ]