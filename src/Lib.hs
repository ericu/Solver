module Lib
    ( test
    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (partition, transpose, intercalate, intersperse)
import Data.List.Split (chunksOf)
import Debug.Trace (traceShow, trace)

test :: IO ()
test = do
  dumpBoardAsRows initialBoard

data Offset = Offset { dc :: Int, dr :: Int } deriving (Show, Eq, Ord)
data Coord = Coord { col :: Int, row :: Int } deriving (Show, Eq, Ord)
type State = Set Int
numberRange = [1..9]
coordRange = [0..8]
type Board = Map Coord State
type Unit = Set Coord

allPossibleCoordsFor :: Board -> Int -> Set Coord
allPossibleCoordsFor b n = allPossibleCoordsInUFor b allCoords n

allPossibleCoordsInUFor :: Board -> Unit -> Int -> Set Coord
allPossibleCoordsInUFor b u n =
  Set.filter (\c -> n `Set.member` (b Map.! c)) u

-- This is the inner part of the interior check.  If in a unit a number appears
-- only in a single coord, then that coord can't hold anything but that number.
clearSolvedInUnitForN :: Int -> Board -> Unit -> Board
clearSolvedInUnitForN n b u =
  let cs = allPossibleCoordsInUFor b u n
  in if Set.size cs == 1
     then
       let [c] = Set.toList cs
       in Map.insert c (Set.singleton n) b
     else b

clearSolvedForN :: Board -> Int -> Board
clearSolvedForN b n = Set.foldl (clearSolvedInUnitForN n) b allUnits

clearSolved :: Board -> Board
clearSolved b = foldl clearSolvedForN b numberRange

clearSolvedIfMoreThan :: Board -> Int -> Board
clearSolvedIfMoreThan b count =
  let b' = clearSolved b
      count' = countSolved b'
  in if count' > count
     then clearSolvedIfMoreThan b count'
     else b

clearSolvedUntilStable :: Board -> Board
clearSolvedUntilStable b =
  let count = countSolved b
  in clearSolvedIfMoreThan b count

{-
clearSolvedUntilStable :: Board -> Board
clearSolvedUntilStable b =
  let initial = countSolved b
      b' = clearSolved b
      current = countSolved b'
  in if current > initial
     then clearSolvedUntilStable b'
     else b
 -}

countSolved :: Board -> Int
countSolved b =
  let solved = filter (\s -> Set.size s == 1) (Map.elems b)
  in length solved

allRows :: Set Unit
allRows = Set.fromList
            [ Set.fromList [ Coord c r | c <- coordRange]
            | r <- coordRange ]
allCols :: Set Unit
allCols = Set.fromList
            [ Set.fromList [Coord c r | r <- coordRange]
            | c <- coordRange ]
allBoxes :: Set Unit
allBoxes = Set.empty -- TODO

allUnits :: Set Unit
allUnits = allRows `Set.union` allCols `Set.union` allBoxes

allCoordsByRow = [Coord c r | r <- coordRange, c <- coordRange]
allCoords = Set.fromList allCoordsByRow

blankBoard :: Board
blankBoard =
  let blank = Set.fromList numberRange
  in Map.fromList $ zip allCoordsByRow $ repeat blank

initialBoard :: Board
initialBoard =
  let b = blankBoard
      b' = setValue (Coord 2 3) 3 b
      b'' = setValue (Coord 4 2) 4 b'
  in b''

-- TODO: Pretty board display
dumpBoard :: Board -> IO ()
dumpBoard b = do
  let kvs = map (\c -> (show c) ++ ": " ++ (show $ b Map.! c))
                allCoordsByRow
  mapM_ putStrLn kvs
  
cellAsRows :: State -> [String]
cellAsRows s =
  let f n = if n `Set.member` s then show n else " "
      allNums = [f n | n <- numberRange]
  in map (intercalate " ") $ chunksOf 3 allNums

boardAsRows :: Board -> [String]
boardAsRows b =
  let f coord = cellAsRows $ b Map.! coord
      allCells = [f coord | coord <- allCoordsByRow]
      allCellRows = chunksOf 9 allCells
      g row = map (intercalate " | ") (transpose row)
      allNumberRows = map g allCellRows
      lineLength = length $ head $ head allNumberRows
      -- TODO: Get this spacer to work.
      spacer = concat $ take lineLength $ repeat "-"
  in concat $ intersperse [spacer] allNumberRows

dumpBoardAsRows :: Board -> IO ()
dumpBoardAsRows b = do
  mapM_ putStrLn $ boardAsRows b

setValue :: Coord -> Int -> Board -> Board
setValue c n b =
  Map.insert c (Set.singleton n) b

isKingsMove :: Offset -> Bool
isKingsMove (Offset dc dr) =
  abs dc <= 1 && abs dr <= 1 && dc /= 0 || dr /= 0

isKnightsMove :: Offset -> Bool
isKnightsMove (Offset dc dr) =
  (abs dc == 1 && abs dr == 2) || (abs dc == 2 && abs dr == 1)

offset :: Coord -> Coord -> Offset
offset (Coord c0 r0) (Coord c1 r1) = Offset (c1 - c0) (r1 - r0)

sees :: Coord -> Coord -> Bool
sees c0 c1 =
  let off = offset c0 c1
  in isKnightsMove off || isKingsMove off ||
     row c0 == row c1 || col c0 == col c1
