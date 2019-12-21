
candy3 :: Int -> [Int] -> Bool
candy3 _ [] = False
candy3 n l = (==0) $ (sum l) `mod` n 

solve :: [Int] -> String
solve [] = ""
solve (h:t) = show (candy3 h (take h t)) ++ "\n" ++ (solve . snd $ (splitAt (h + 1) t))

main = do  
    contents <- getContents  
    putStr $ solve $ map (\x -> read x :: Int) (tail . words $ contents)

