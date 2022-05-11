import Data.Char

transform :: String -> String
transform = map toUpper . filter isLetter . reverse 