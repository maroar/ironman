solve :: String -> [String] -> String
solve str   []   = if str=="42" then "" else str
solve str (h:t)
  | str == "42" = ""
  | otherwise = str ++ "\n" ++ solve h t

main = do  
    contents <- getContents  
    putStr $ (solve (head (words contents)) (tail (words contents)) )