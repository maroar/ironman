solve :: [(String, Int)] -> String -> String
solve students n = "Instancia " ++ n ++ "\n" ++ reproved students

ins :: (String, Int) -> [(String, Int)] -> [(String, Int)]
ins a []      = [a]
ins (n1, s1) ((n2,s2):t)
  | s1 < s2   = (n1, s1) : (n2, s2) : t
  | s1 > s2   = (n2, s2) : ins (n1, s1) t
  | n1 > n2   = (n1, s1) : (n2, s2) : t
  | otherwise = (n2, s2) : ins (n1, s1) t

iSort :: [(String, Int)] -> [(String, Int)]
iSort []    = []
iSort (h:t) = ins h (iSort t) 

reproved :: [(String, Int)] -> String
reproved [] = "" 
reproved l  = (fst lastStudent) ++ "\n" 
  where lastStudent = head (iSort l)

convertInput :: [String] -> [(String, Int)]
convertInput [] = []
convertInput (name:y:t) = (name, score) : convertInput t
  where score = read y :: Int

spoj_PLACAR :: [String] -> Int -> String
spoj_PLACAR [] instanceNumber = ""
spoj_PLACAR input instanceNumber = 
  let
    n          = read (head input) :: Int
    dados      = convertInput (take (n*2) (drop 1 input))
    instNumStr = show instanceNumber
  in
    (solve dados instNumStr) ++ 
      spoj_PLACAR (drop (1+2*n) input) (instanceNumber + 1)

main = do  
    contents <- getContents  
    putStr $ spoj_PLACAR (words contents) 1
