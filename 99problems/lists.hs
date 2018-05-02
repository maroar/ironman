-- 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (h:[]) = Just h
myLast (_:t) = myLast t

-- 2
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (l:[]) = Nothing
myButLast (bl:l:[]) = Just bl
myButLast (_:t) = myButLast t

-- 3
elementAt :: [a] -> Int -> Maybe a
elementAt [] n = Nothing
elementAt (h:t) n = if (n <= 0) then Nothing else if (n == 1) then Just h else elementAt t (n-1)

-- 4
myLenght :: [a] -> Int
myLenght = foldr (\_ acc -> acc + 1) 0

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc e -> e : acc) []

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == (myReverse l)

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List l) = foldr (\e acc -> (flatten e) ++ acc) [] l

-- 8
decide :: Eq a => a -> [a] -> [a] -> [a]
decide e acc [] = e:acc
decide e acc (h:t) = if (e == h) then (decide e acc t) else (decide h (e:acc) t) 

compress :: Eq a => [a] -> [a]
compress [] = []
compress (h:t) = reverse $ decide h [] t 

-- 9
agregate :: Eq a => a -> ([a], [[a]]) -> [a] -> [[a]]
agregate e (acc, total) [] = (e:acc):total
agregate e (acc, total) (h:t) = if (e == h) then (agregate e (h:acc, total) t) else (agregate h ([], (e:acc):total) t) 

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (h:t) = reverse $ agregate h ([], []) t

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\e -> (length e, head e)) . pack

-- 11
data Multiplicity a = Multiple Int a | Single a deriving (Eq, Show)

encodeModified :: Eq a => [a] -> [Multiplicity a]
encodeModified = map (\(t, e) -> if t > 1 then (Multiple t e) else (Single e)) . encode

-- 12
translateMultiplicity :: Multiplicity a -> Int
translateMultiplicity (Single _) = 1
translateMultiplicity (Multiple n _) = n

valueMultiplicity :: Multiplicity a -> a
valueMultiplicity (Single e) = e
valueMultiplicity (Multiple _ e) = e

--decodeModified :: [Multiplicity a] -> [a]
--decodeModified = map (\e -> (take valueMultiplicity e) $ repeat $ translateMultiplicity e) 

-- 13

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (h:t) = h:h:dupli t
