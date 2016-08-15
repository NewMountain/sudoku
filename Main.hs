module Main where

import Data.List


type Row = [ Maybe Int ]
-- Todo limit int to 0-9

type Board = [Row]
-- Todo

-- At each row, create a row of all possibilities that sum to $ sum [1..9]
-- do not replace any existing data, only Nothing
-- At each column, do not pick any numbers already used

solveSudoku :: Board -> [Board]
solveSudoku board =
  let
    options = possibilitiesMatrix board
    beastMatrix = sequence options
  in
    filter sudokuTest beastMatrix


sudokuTest :: [Row] -> Bool
sudokuTest matrix =
  let
    rowTest = all (\x -> fst x == 1) $ concatMap frequency matrix
    colTest = all (\x -> fst x == 1) $ concatMap frequency $ transpose matrix
  in
    (rowTest && colTest)


frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))


possibilitiesMatrix :: Board -> [[Row]]
possibilitiesMatrix board =
  let
    side = length board - 1
  in
    fmap (solutionFilter board) [0..side]


solutionFilter :: Board -> Int ->[Row]
solutionFilter board i =
  filter (constraintChecker i board) (rowCombinator board i)


constraintChecker :: Int -> Board -> Row -> Bool
constraintChecker i initBoard row =
  let
    constraints = getColumnConstraints $ colConstraints i initBoard
    intRow = takenSlots row
    test = zip intRow constraints
  in
    all (\(x, y) -> notElem x y) test


getColumnConstraints :: Board -> [[Int]]
getColumnConstraints board =
  takenSlots <$> transpose board


-- This was a weird twist. You can't filter on constraints in your row
-- Those are properties of the board itself and already resolved using
-- Row combinator
colConstraints :: Int -> Board -> Board
colConstraints row board =
  let
    filterRows n board =
      take n board ++ drop (n+1) board
  in
    filterRows row board


rowCombinator :: Board -> Int -> [Row]
rowCombinator board i =
  let
    row = board !! i
  in
    fmap (mergeRowAndUnusedList row) (allUnusedOptions row)


mergeRowAndUnusedList :: Row -> [Int] -> Row
mergeRowAndUnusedList = go []
  where
    go acc r l =
      case (r, l) of
        (Just r:rs, x:xs) -> go ( acc ++ [Just r] ) rs (x:xs)
        (Nothing:rs, x:xs) -> go ( acc ++ [Just x] ) rs xs
        (r:rs, []) -> go (acc ++ [r]) rs []
        ([], _) -> acc


allUnusedOptions :: Row -> [[Int]]
allUnusedOptions row = permutations $ remainingOptions row


remainingOptions :: Row -> [Int]
remainingOptions row =
  let
    leftovers = takenSlots row
  in
    filter (`notElem` leftovers) [1..9]


takenSlots :: Row -> [Int]
takenSlots row = map (\ (Just a) -> a) $ filter (/= Nothing) row


initBoard :: Board
initBoard
  = [ [Just 5, Just 3, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing ]
    , [Just 6, Nothing, Nothing, Just 1, Just 9, Just 5, Nothing, Nothing, Nothing ]
    , [Nothing, Just 9, Just 8, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing ]
    , [Just 8, Nothing, Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Just 3]
    , [Just 4, Nothing, Nothing, Just 8, Nothing, Just 3, Nothing, Nothing, Just 1]
    , [Just 7, Nothing, Nothing, Nothing, Just 2, Nothing, Nothing, Nothing, Just 6]
    , [Nothing, Just 6, Nothing, Nothing, Nothing, Nothing, Just 2, Just 8, Nothing]
    , [Nothing, Nothing, Nothing, Just 4, Just 1, Just 9, Nothing, Nothing, Just 5]
    , [Nothing, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Just 7, Just 9]
    ]


main =
  print $ solveSudoku initBoard
