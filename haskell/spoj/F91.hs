f91 n
  | n <= 100  = f91 (f91 (n + 11))
  | otherwise = n - 10 

solve []    = ""
solve (h:t) = 
  let 
    n = read h :: Int 
  in 
    if n /= 0 then
      "f91(" ++ h ++ ") = " ++ show ((f91 n)) ++ "\n" ++ solve t
    else
      ""  

main = do  
    contents <- getContents  
    putStr $ (solve (words contents))