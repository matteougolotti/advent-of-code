main :: IO()
main = interact $ (\d -> show (firstPart d, secondPart d)) . map words . lines

firstPart :: [[String]] -> Int
firstPart = foldr (\ [opponentsMove, myMove] acc ->
                    acc + case (opponentsMove, myMove) of ("A", "X") -> 4
                                                          ("A", "Y") -> 8
                                                          ("A", "Z") -> 3
                                                          ("B", "X") -> 1
                                                          ("B", "Y") -> 5
                                                          ("B", "Z") -> 9
                                                          ("C", "X") -> 7
                                                          ("C", "Y") -> 2
                                                          ("C", "Z") -> 6
                                                          _ -> 0
                    ) 0

secondPart :: [[String]] -> Int
secondPart = foldr (\ [opponentsMove, myMove] acc ->
                    acc + case (opponentsMove, myMove) of ("A", "X") -> 3
                                                          ("A", "Y") -> 4
                                                          ("A", "Z") -> 8
                                                          ("B", "X") -> 1
                                                          ("B", "Y") -> 5
                                                          ("B", "Z") -> 9
                                                          ("C", "X") -> 2
                                                          ("C", "Y") -> 6
                                                          ("C", "Z") -> 7
                                                          _ -> 0
                    ) 0
