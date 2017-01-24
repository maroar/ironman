

infoContent :: Float -> Float
infoContent p = log (1/p) / log 2

entropy :: [Float] -> Float
entropy = sum . map (\p -> p * infoContent p)

condEntropy :: [Float] -> [Float] -> Float
condEntropy p_xy p_xIy = sum $ zipWith (*) p_xy (map infoContent p_xIy)

normalize :: [Float] -> Float -> [Float]
normalize l v = map (/v) l