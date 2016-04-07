initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname
          
maximum' :: (Ord a) => [a] -> a 
maximum' []    = error "ERROR: empty list!"
maximum' [a]   = a
maximum' (h:t)
  | h > maxElement   = h
  | otherwise        = maxElement
   where maxElement = maximum' t
   
maximum'' :: (Ord a) => [a] -> a 
maximum'' []    = error "ERROR: empty list!"
maximum'' [a]   = a
maximum'' (h:t) = max h (maximum'' t)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' 0 _     = []
take' _ []    = []
take' n (h:t) = h : (take' (n-1) (t))

-- foldl starts here (a1) -> [a1, a2, a3, ..., an] <- foldr starts here (an)

sum' :: (Num a) => [a] -> a
sum' l = foldl (\acc x -> acc + x) 0 l

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' a = foldl (\acc x -> (x == a) || acc ) False

map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\x acc -> (f x) : acc) [] l

maximum''' :: (Ord a) => [a] -> a 
maximum''' [] = error "ERROR: empty list!"
maximum''' l  = foldl (\acc x -> if acc > x then acc else x) (head l) (tail l)

reverse' :: [a] -> [a]
reverse' l = foldl (\acc x -> x : acc) [] l

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if (f x) then (x : acc) else acc) []


