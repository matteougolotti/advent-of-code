import Data.Char
import Data.Map

main :: IO()
main = interact $ show . sum . Prelude.map toInt . Prelude.map commonChar . Prelude.map half . lines

commonChar :: (String, String) -> Char
commonChar (l, r) = head $ Data.Map.elems $ Data.Map.intersection (Data.Map.fromList (zip l l)) (Data.Map.fromList (zip r r))

toInt :: Char -> Int
toInt c = if r >= 97 then r - 96 else r - 38 where r = Data.Char.ord c

half :: [a] -> ([a], [a])
half list = Prelude.splitAt (length list `div` 2) list
