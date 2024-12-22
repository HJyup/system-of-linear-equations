module Module.Matrix.Type where

newtype Matrix = Matrix [[Double]]
    deriving (Show, Eq)

instance Num Matrix where
    (+) (Matrix a) (Matrix b)
        | isValid a && isValid b && sameSize a b =
            Matrix (zipWith (zipWith (+)) a b)
        | otherwise =
            error "(+): dimensions mismatch or invalid"

    (-) (Matrix a) (Matrix b)
        | isValid a && isValid b && sameSize a b =
            Matrix (zipWith (zipWith (-)) a b)
        | otherwise =
            error "(-): dimensions mismatch or invalid"

    (*) (Matrix a) (Matrix b)
        | isValid a && isValid b && canMultiply a b =
            let bT = toList $ transposeMatrix (Matrix b)
            in Matrix [[sum (zipWith (*) row col) | col <- bT] | row <- a]
        | otherwise =
            error "(*): dimensions mismatch or invalid"
      where
        canMultiply :: [[Double]] -> [[Double]] -> Bool
        canMultiply rowsA rowsB =
            not (null rowsA) && not (null rowsB) && length (head rowsA) == length rowsB

    negate (Matrix a) = Matrix (map (map negate) a)
    abs    (Matrix a) = Matrix (map (map abs) a)
    signum (Matrix a) = Matrix (map (map signum) a)
    fromInteger x      = Matrix [[fromInteger x]]

transposeMatrix :: Matrix -> Matrix
transposeMatrix (Matrix rows) = Matrix (transpose rows)
  where
    transpose []         = []
    transpose ([] : _ )  = []
    transpose xs         = map head xs : transpose (map tail xs)

fromList :: [[Double]] -> Maybe Matrix
fromList xs
    | isValid xs = Just (Matrix xs)
    | otherwise  = Nothing

toList :: Matrix -> [[Double]]
toList (Matrix xs) = xs

isValid :: [[Double]] -> Bool
isValid rows =
    case rows of
      []     -> True
      (r:rs) -> all ((== length r) . length) rs

sameSize :: [[Double]] -> [[Double]] -> Bool
sameSize a b =
    length a == length b &&
    length (head a) == length (head b)

scalarMultiply :: Double -> Matrix -> Matrix
scalarMultiply k (Matrix rows) =
  Matrix (map (map (k *)) rows)

isSquare :: [[Double]] -> Bool
isSquare []   = True
isSquare rows =
  let rowCount = length rows
      colCount = length (head rows)
  in  rowCount == colCount &&
      all (\r -> length r == colCount) rows