module Module.Parse where

import Data.List.Split (splitOn)
import Data.List.Utils (replace)

import Debug.Trace (trace)

normalize :: String -> String
normalize = filter (not . (`elem` " \t"))

extractCoefficients :: String -> [Double]
extractCoefficients =
    map parseTerm
    . filter (not . null)
    . splitOn "+"
    . replace "-" "+-"
    . takeWhile (/= '=')
    . normalize

extractAnswer :: String -> [Double]
extractAnswer =
    map parseTerm
    . filter (not . null)
    . splitOn "+"
    . replace "-" "+-"
    . drop 1
    . dropWhile (/= '=')
    . normalize

parseTerm :: String -> Double
parseTerm term =
    case filter (`notElem` "xyz") term of
        ""     -> if head term == '-' then -1 else 1
        noVars -> read noVars