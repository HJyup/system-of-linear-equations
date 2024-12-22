module Main where

import Module.Matrix

main :: IO ()
main = do
    let m1 = fromList [[1, 2], [3, 4]]
    let m2 = fromList [[5, 6], [7, 8]]
    let m3 = fromList [[1, 1, 1], [5, 5, 5], [7, 8, 121]]

    case (m1, m2, m3) of
        (Just matrix1, Just matrix2, Just matrix3) -> do
            print $ determinant matrix3
            print "Matrices are valid."
        _ -> putStrLn "Invalid matrices"
