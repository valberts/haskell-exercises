import Data.Char
import Test.QuickCheck

code :: Char -> Char
code c
  | c < 'A' || c > 'Z' || c < 'a' || c > 'z'                          = c
  | (ord c) + 3 > (ord 'z') || ((ord c) + 3 > (ord 'Z') && c <= 'Z')  = chr (ord c + 3 - 26)
  | otherwise                                                         = chr (ord c + 3)

code' :: Int -> Char -> Char
code' n c
  | c < 'A' || c > 'Z' || c < 'a' || c > 'z'                          = c
  | (ord c) + n > (ord 'z') || ((ord c) + n > (ord 'Z') && c <= 'Z')  = chr (ord c + n - 26)
  | otherwise                                                         = chr (ord c + n)

prop_eq c = code c == (code' 3) c
prop_orig n c = c == code' (26-n) (code' n c)