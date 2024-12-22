module Main where

import Module.Matrix (fromList, determinant, inverse)
import Module.File (readFileContents, splitLines)
import Module.Parse (extractCoefficients, extractAnswer)

main :: IO ()
main = do
    let path = "data/linear_equation_ordered.txt"
    contents <- readFileContents path
    let lines = splitLines contents
    putStrLn $ "\n" ++ "List of equations: " ++ show lines ++ "\n"

    let coefficients = map extractCoefficients lines
    putStrLn $ "List of coefficients: " ++ show coefficients ++ "\n"

    let answers = map extractAnswer lines
    putStrLn $ "List of answers: " ++ show answers ++ "\n"

    let maybeMatrix  = fromList coefficients
    let maybeAnswers = fromList answers

    case (maybeMatrix, maybeAnswers) of
      (Just matrix, Just answerMatrix) -> do
        putStrLn $ "Matrix A: " ++ show matrix ++ "\n"
        let d = determinant matrix
        putStrLn $ "det(A) = " ++ show d ++ "\n"

        let invA = inverse matrix
        putStrLn $ "Inverse A: " ++ show invA ++ "\n"

        let equationAnswer = invA * answerMatrix
        putStrLn $ "Answer: " ++ show equationAnswer ++ "\n"

      _ -> putStrLn "Invalid matrix data."
