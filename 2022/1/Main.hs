import Data.List

main :: IO()
main = interact $ \d -> show [firstPart d, secondPart d]

toInt :: String -> Int
toInt = read

split :: [String] -> [[String]]
split l = snd $ foldr split' ([], []) l
    where
        split' :: String -> ([String], [[String]]) -> ([String], [[String]])
        split' [] ([], r) = ([], r)
        split' [] (p, r)  = ([], p:r)
        split' s (p, r)   = (s:p, r)

firstPart :: String -> String
firstPart = show . maximum . map (sum . map toInt) . split . lines

secondPart :: String -> String
secondPart = show . sum . take 3 . reverse . Data.List.sort . map (sum . map toInt) . split . lines
