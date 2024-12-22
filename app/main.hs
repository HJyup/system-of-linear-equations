module Main where

import Module.Matrix (fromList, determinant, inverse)

main :: IO ()
main = do
    let maybeA = fromList [[2, 1],
                           [3, 4]]
    case maybeA of
      Nothing -> putStrLn "Invalid matrix data."
      Just a  -> do
        putStrLn $ "Matrix A: " ++ show a ++ "\n"
        let d = determinant a
        putStrLn $ "det(A) = " ++ show d ++ "\n"

        let invA = inverse a
        putStrLn $ "Inverse A: " ++ show invA ++ "\n"

        putStrLn $ "A * A^{-1} = " ++ show (a * invA) ++ "\n"
