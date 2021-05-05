module Sudoku where

import Data.Char (digitToInt, intToDigit)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex, findIndex)
import Data.List.Split (chunksOf)

-------------------------------------------------------------------------

{-| A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
each value is either `Nothing' or `Just n', for some Int value `n'. |-}
newtype Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

{-| A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
or a square. |-}
type Block = [Maybe Int]

{-| A Pos is a zero-based (row, column) position within the puzzle. |-}
newtype Pos = Pos (Int, Int) deriving (Show, Eq)

{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

example :: Puzzle
example =
  Puzzle
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

{-| Ex 1.1

    A sudoku with just blanks. |-}
allBlankPuzzle :: Puzzle
allBlankPuzzle = Puzzle (replicate 9 (replicate 9 Nothing))


{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle puzzle = length (rows puzzle) == 9 && and [length row == 9 | row <- rows puzzle] && and [foreach row | row <- rows puzzle] where
    foreach row = and [foreach' cell | cell <- row] where
        foreach' Nothing = True
        foreach' (Just number) = number <= 9 && number>=1
{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
isSolved puzzle = and [Nothing `notElem` row | row <- rows puzzle]

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}
-- drawPuzzle puzzle = 
drawPuzzle puzzle = concat [foreach row ++ "\n" | row <- rows puzzle] where
    foreach row = [foreach' cell | cell <- row] where
        foreach' Nothing =  '.'
        foreach' (Just number) = intToDigit number

printPuzzle :: Puzzle -> IO ()
printPuzzle puzzle = putStrLn (drawPuzzle puzzle)

{-| Ex 2.2
    -- sud <- readFile "puzzles/easy1.sud"
    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}
formatString :: String -> Block
formatString [] = []
formatString (x:xs) = case x of
    '.' -> Nothing : formatString xs
    '\n' -> formatString xs
    _ -> Just (digitToInt x) : formatString xs

readPuzzle :: FilePath -> IO Puzzle
readPuzzle path = readFile path >>= pure . (\ x -> if (isPuzzle) x then x else error "File Doesn't Contain Valid Sudoku Puzzle") . Puzzle . map formatString . lines

{-| Ex 3.1
    -- isValidBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2] = True
    -- isValidBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 1] = False
    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock [] = True
isValidBlock (Nothing:xs) = isValidBlock xs
isValidBlock (x:xs) = if notElem x xs then isValidBlock xs else False

{-| Ex 3.2
    
    Collect all blocks on a board - the rows, the columns and the squares. |-}

    {- example =
    [
    [Just 3,Just 6,Nothing,Nothing,Just 7,Just 1,Just 2,Nothing,Nothing],
    [Nothing,Just 5,Nothing,Nothing,Nothing,Nothing,Just 1,Just 8,Nothing],
    [Nothing,Nothing,Just 9,Just 2,Nothing,Just 4,Just 7,Nothing,Nothing],
    [Nothing,Nothing,Nothing,Nothing,Just 1,Just 3,Nothing,Just 2,Just 8],
    [Just 4,Nothing,Nothing,Just 5,Nothing,Just 2,Nothing,Nothing,Just 9],
    [Just 2,Just 7,Nothing,Just 4,Just 6,Nothing,Nothing,Nothing,Nothing],
    [Nothing,Nothing,Just 5,Just 3,Nothing,Just 8,Just 9,Nothing,Nothing],
    [Nothing,Just 8,Just 3,Nothing,Nothing,Nothing,Nothing,Just 6,Nothing],
    [Nothing,Nothing,Just 7,Just 6,Just 9,Nothing,Nothing,Just 4,Just 3],

    [Just 3,Nothing,Nothing,Nothing,Just 4,Just 2,Nothing,Nothing,Nothing],
    [Just 6,Just 5,Nothing,Nothing,Nothing,Just 7,Nothing,Just 8,Nothing],
    [Nothing,Nothing,Just 9,Nothing,Nothing,Nothing,Just 5,Just 3,Just 7],
    [Nothing,Nothing,Just 2,Nothing,Just 5,Just 4,Just 3,Nothing,Just 6],
    [Just 7,Nothing,Nothing,Just 1,Nothing,Just 6,Nothing,Nothing,Just 9],
    [Just 1,Nothing,Just 4,Just 3,Just 2,Nothing,Just 8,Nothing,Nothing],
    [Just 2,Just 1,Just 7,Nothing,Nothing,Nothing,Just 9,Nothing,Nothing],
    [Nothing,Just 8,Nothing,Just 2,Nothing,Nothing,Nothing,Just 6,Just 4],
    [Nothing,Nothing,Nothing,Just 8,Just 9,Nothing,Nothing,Nothing,Just 3],

    [Just 3,Just 6,Nothing,Nothing,Just 5,Nothing,Nothing,Nothing,Just 9],
    [Nothing,Just 7,Just 1,Nothing,Nothing,Nothing,Just 2,Nothing,Just 4],
    [Just 2,Nothing,Nothing,Just 1,Just 8,Nothing,Just 7,Nothing,Nothing],
    [Nothing,Nothing,Nothing,Just 4,Nothing,Nothing,Just 2,Just 7,Nothing],
    [Nothing,Just 1,Just 3,Just 5,Nothing,Just 2,Just 4,Just 6,Nothing],
    [Nothing,Just 2,Just 8,Nothing,Nothing,Just 9,Nothing,Nothing,Nothing],
    [Nothing,Nothing,Just 5,Nothing,Just 8,Just 3,Nothing,Nothing,Just 7],
    [Just 3,Nothing,Just 8,Nothing,Nothing,Nothing,Just 6,Just 9,Nothing],
    [Just 9,Nothing,Nothing,Nothing,Just 6,Nothing,Nothing,Just 4,Just 3]
    ]
    -}
blocks :: Puzzle -> [Block]
blocks puzzle = row ++ transpose row ++ (map concat . concatMap transpose . chunksOf 3 . map (chunksOf 3)) row
    where row = rows puzzle

--     (rows puzzle) ++ (transpose (rows puzzle)) ++ squares puzzle where
--     squares puzzle = [square (x,y) puzzle | y <- [0..2], x <- [0..2]]

-- row :: Int -> Puzzle -> Block
-- row int puzzle = (rows puzzle) !! int

-- square :: (Int, Int) -> Puzzle -> Block
-- square (x,y) puzzle = 
--       concat
--       $ [take 3 (drop (x*3) row) | row <- take 3 (drop (y*3) (rows puzzle))]

{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle puzzle = and [isValidBlock block | block <- (blocks puzzle)]

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank puzzle = case (findIndex isJust indexList) of Just x -> Pos (x, fromJust (indexList !! x))
    where indexList = [elemIndex Nothing row | row <- rows puzzle]
                   
{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) list (index, value) = if index < 0 || index > (length list)-1 then list else (take index list) ++ [value] ++ (drop (index+1) list)

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update puzzle (Pos(x,y)) value = Puzzle $ take x (rows puzzle) ++ [row !!= (y, value)] ++ drop (x+1) (rows puzzle)
    where row = (rows puzzle) !! x

{-| Ex 5.1

    Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve = undefined

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO (Maybe Puzzle)
readAndSolve = undefined

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
isSolutionOf :: Puzzle -> Puzzle -> Bool
isSolutionOf = undefined