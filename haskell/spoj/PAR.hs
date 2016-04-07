solve :: String -> String -> [Int] -> String -> String
solve p1 p2 games n = "Teste " ++ n ++ "\n" ++ listOfWinners p1 p2 games 

listOfWinners :: String -> String -> [Int] -> String
listOfWinners _ _ [] = "\n"
listOfWinners p1 p2 (n1:n2:t)
  | ((n1+n2) `mod` 2) == 0 = p1 ++ "\n" ++ listOfWinners p1 p2 (t)
  | otherwise              = p2 ++ "\n" ++ listOfWinners p1 p2 (t)

spoj_PAR :: [String] -> Int -> String
spoj_PAR input instanceNumber = 
  let
      nChar = head input
  in
      if nChar == "0" then 
        ""
      else
        let
          n                 = read nChar :: Int
          player1           = head (drop 1 input)
          player2           = head (drop 2 input)
          games             = map (\x -> read x :: Int) (take (n*2) (drop 3 input))
          instanceNumberStr = show instanceNumber
        in
          (solve player1 player2 games instanceNumberStr) ++ spoj_PAR (drop (3+2*n) input) (instanceNumber + 1)

main = do  
    contents <- getContents  
    putStr $ spoj_PAR (words contents) 1
