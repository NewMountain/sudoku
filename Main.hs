module Main where


type Row = [ Maybe Int ]
-- Todo limit int to 0-9

type Board = [Row]
-- Todo

-- At each row, create a row of all possibilities that sum to $ sum [1..9]
-- do not replace any existing data, only Nothing
-- At each column, do not pick any numbers already used


sampleRow :: Row
sampleRow = [Just 5, Just 3, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing ]


takenSlots :: Row -> [Int]
takenSlots row = map maybePlucker $ filter (/= Nothing) row


maybePlucker :: Maybe Int -> Int
maybePlucker (Just a) = a


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
  print initBoard
