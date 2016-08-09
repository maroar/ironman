import Data.Char as C

data Info = Info String

data Term = CTrue Info
          | CFalse Info
          | If Info Term Term Term
          | CZero Info
          | Succ Info Term
          | Pred Info Term
          | Iszero Info Term

isnumericalvalue :: Term -> Bool
isnumericalvalue (CZero _) = True
isnumericalvalue (Succ _ t) = isnumericalvalue t
isnumericalvalue _ = False

main = do
  input <- getContents
  putStr (map C.toUpper input) 
