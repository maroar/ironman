data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

type Tournament = ([Move], [Move])

let tournamentExample = ([Rock, Rock, Paper],[Scissors, Paper, Rock])

--8.1
outcome :: Move -> Move -> Integer
outcome move1 move2
  | beat move1 == move2 = -1
  | beat move2 == move1 = 1
  | otherwise           = 0

--8.2
tournamentOutcome :: Tournament -> Integer
tournamentOutcome 

main = do
  putStr (show(outcome Rock Scissors) ++ "\n")