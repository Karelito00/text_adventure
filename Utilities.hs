module Utilities where

import Data.Char

isInt :: String -> Bool
isInt [] = True
isInt (x:xs)
    | isDigit x = isInt xs
    | otherwise = False

parseStringToInt :: String -> Int -> Int
parseStringToInt [] val = val
parseStringToInt (x:xs) val
    | isDigit x = parseStringToInt xs (val * 10 + (digitToInt x))
    | otherwise = -1

