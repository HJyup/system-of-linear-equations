module Main where

import Module.Matrix

main :: IO ()
main = do
    let m1 = fromList [[1, 2], [3, 4]]
    let m2 = fromList [[5, 6], [7, 8]]

    case (m1, m2) of
        (Just matrix1, Just matrix2) -> do
            print (matrix1 + matrix2)
            print (matrix1 - matrix2)
        _ -> putStrLn "Invalid matrices"
