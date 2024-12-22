module Module.Matrix (
    Matrix,
    fromList,
    toList
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
        isScalarMatrix [[_]] = True
        isScalarMatrix _     = False
        scalarValue [[x]] = x
        scalarValue _     = error "Invalid scalar matrix"

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
