removeF _ []    = [] 
removeF n (h:t) 
  | h < n     = removeF n t
  | otherwise = (h:t) 

remove _ [] = []
remove n l  = [ x | x <- l, (x `mod` n /= 0)]

crivo []    = []
crivo (h:t) = h : crivo (remove h t)

getPrimes n1 n2
  | n1 <= 2           = removeF n1 (2 : crivo [3,5..n2])
  | otherwise         = removeF n1 (crivo [3,5..n2])

getFormatedPrimes n1 n2 = format $ vetToString $ getPrimes n1 n2 

format []    = ""
format (h:t) = h ++ "\n" ++ format t

vetToString l = map (show) l

solve []        = ""
solve (h1:h2:t) = 
  let 
    n1 = read h1 :: Int
    n2 = read h2 :: Int
  in
    (getFormatedPrimes n1 n2) ++ "\n" ++ solve t

main = do  
    contents <- getContents  
    putStr $ (solve (tail (words contents)))