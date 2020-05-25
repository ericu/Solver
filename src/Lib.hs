module Lib
    ( test
    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (partition, transpose, intercalate, intersperse, any, all)
import Data.List.Split (chunksOf)
import Debug.Trace (traceShow, trace)

testBox = Set.fromList [ Coord (0 + c) (3 + r) | c <- [0, 1, 2], r <- [0, 1, 2] ]

test :: IO ()
test = do
--  dumpBoard initialBoard
--  dumpBoard $ clearSeenForNFromUnit 3 (clearOutAllUnitsForN initialBoard 3)
--                                    testBox
--  dumpBoard $ clearOutAllUnitsForN initialBoard 3
--  dumpBoard $ (clearSeenForAllN) $
--              (clearOutAllUnitsForAllN) initialBoard
  dumpBoard $ doYourBest initialBoard

data Offset = Offset { dc :: Int, dr :: Int } deriving (Show, Eq, Ord)
data Coord = Coord { col :: Int, row :: Int } deriving (Show, Eq, Ord)
type State = Set Int
numberRange = [1..9]
coordRange = [0..8]
type Board = Map Coord State
type Unit = Set Coord

allRows :: Set Unit
allRows = Set.fromList
            [ Set.fromList [ Coord c r | c <- coordRange]
            | r <- coordRange ]
allCols :: Set Unit
allCols = Set.fromList
            [ Set.fromList [Coord c r | r <- coordRange]
            | c <- coordRange ]
allBoxes :: Set Unit
allBoxes = Set.fromList [
             Set.fromList [Coord (c + dc) (r + dr) | c <- [0..2], r <- [0..2]]
                           | dc <- [0, 3, 6], dr <- [0, 3, 6] ]

allUnits :: Set Unit
allUnits = allRows `Set.union` allCols `Set.union` allBoxes

allCoordsByRow = [Coord c r | r <- coordRange, c <- coordRange]
allCoords = Set.fromList allCoordsByRow

forAllN :: (Board -> Int -> Board) -> Board -> Board
forAllN f b = foldl f b numberRange

forAllUnits :: (Board -> Unit -> Board) -> Board -> Board
forAllUnits f b = Set.foldl f b allUnits

forAllUnitsAndN :: (Int -> Board -> Unit -> Board) -> Board -> Board
forAllUnitsAndN f b = forAllN (\board n -> forAllUnits (f n) board) b

blankBoard :: Board
blankBoard =
  let blank = Set.fromList numberRange
  in Map.fromList $ zip allCoordsByRow $ repeat blank

setValue :: Coord -> Int -> Board -> Board
setValue c n b =
  Map.insert c (Set.singleton n) b

initialBoard :: Board
initialBoard =
  let b = blankBoard
      b' = setValue (Coord 2 3) 3 b
      b'' = setValue (Coord 4 2) 4 b'
  in b''

countPossibilities :: Board -> Int
countPossibilities b =
  let addSet soFar s = soFar + Set.size s
  in Map.foldl addSet 0 b

allPossibleCoordsInUFor :: Board -> Unit -> Int -> Set Coord
allPossibleCoordsInUFor b u n =
  Set.filter (\c -> n `Set.member` (b Map.! c)) u

removeNFromCoord :: Int -> Board -> Coord -> Board
removeNFromCoord n b c =
  let state = b Map.! c
      state' = Set.delete n state
  in Map.insert c state' b

isSingleN :: Int -> Coord -> Board -> Bool
isSingleN n c b = b Map.! c == Set.singleton n

isSingleton :: Board -> Coord -> Bool
isSingleton b c = Set.size (b Map.! c) == 1

getSingleton :: Board -> Coord -> Int
getSingleton b c =
  let allSingles = Set.filter (isSingleton b) allCoords
  in if isSingleton b c
     then Set.findMin (b Map.! c)
     else error "Not a singleton!"

-- If in a unit a number appears only in a single coord, then that coord can't
-- hold anything but that number.
clearOwnedCellInUnitForN :: Int -> Board -> Unit -> Board
clearOwnedCellInUnitForN n b u =
  let cs = allPossibleCoordsInUFor b u n
  in if Set.size cs == 1
     then
       let [c] = Set.toList cs
       in Map.insert c (Set.singleton n) b
     else b

clearOwnedCellForAllN :: Board -> Board
clearOwnedCellForAllN = forAllUnitsAndN clearOwnedCellInUnitForN

-- If you've got a singleton for a number [i.e. that must be where that number
-- is], then clear that number from all other coords in the unit.
clearOutUnitForN :: Int -> Board -> Unit -> Board
clearOutUnitForN n b u =
  let listOfSingles = Set.toList $ Set.filter (\c -> isSingleN n c b) u
  in case listOfSingles of
       [c] -> let otherCoords = u `Set.difference` (Set.singleton c)
              in Set.foldl (removeNFromCoord n) b otherCoords
       a:b:c -> error "Unexpected Conflict"
       _ -> b

clearOutAllUnitsForAllN :: Board -> Board
clearOutAllUnitsForAllN = doUntilStable $ forAllUnitsAndN clearOutUnitForN

-- If all possibilities for a number in a unit can "see" a coord in
-- another unit [via knight's move, king's move, or sudoku rules], then that
-- coord isn't a possibility for that number.

isKingsMove :: Offset -> Bool
isKingsMove (Offset dc dr) =
  abs dc <= 1 && abs dr <= 1 && (dc /= 0 || dr /= 0)

isKnightsMove :: Offset -> Bool
isKnightsMove (Offset dc dr) =
  (abs dc == 1 && abs dr == 2) || (abs dc == 2 && abs dr == 1)

getOffset :: Coord -> Coord -> Offset
getOffset (Coord c0 r0) (Coord c1 r1) = Offset (c1 - c0) (r1 - r0)

addOffset :: Coord -> Offset -> Coord
addOffset (Coord c r) (Offset dc dr) = Coord (c + dc) (r + dr)

sees :: Coord -> Coord -> Bool
sees c0 c1 =
  let off = getOffset c0 c1
  in isKingsMove off || isKnightsMove off ||
     row c0 == row c1 || col c0 == col c1
clearSeenForNFromUnit :: Int -> Board -> Unit -> Board
clearSeenForNFromUnit n b u =
  let isNPossibleAtCoord c = Set.member n $ b Map.! c
      allNLocations = Set.filter isNPossibleAtCoord allCoords
      nInUnit = allNLocations `Set.intersection` u
      nOutOfUnit = allNLocations `Set.difference` nInUnit
      seenFromAllInUnit c = all (sees c) nInUnit
      f board c = if seenFromAllInUnit c
                  then removeNFromCoord n board c
                  else board
  in foldl f b nOutOfUnit

clearSeenForAllN :: Board -> Board
clearSeenForAllN = doUntilStable $ forAllUnitsAndN clearSeenForNFromUnit

-- Numbers can't be orthogonally next to adjacent integers [e.g. 4 can't be next
-- to a 3 or a 5], regardless of unit.

isLegalCoord :: Coord -> Bool
isLegalCoord (Coord c r) = elem c coordRange && elem r coordRange

orthoOffsets = [Offset 0 1, Offset 0 (-1), Offset 1 0, Offset (-1) 0]

allLegalOrthoNeighbors :: Coord -> Set Coord
allLegalOrthoNeighbors c =
  Set.fromList $ filter isLegalCoord $ map (addOffset c) orthoOffsets

clearOrthoNeighborsForSingleton :: Board -> Coord -> Board
clearOrthoNeighborsForSingleton b c =
  let cs = allLegalOrthoNeighbors c
      n = getSingleton b c
      removeBoth board coord =
        let board' = removeNFromCoord (n - 1) board coord
        in removeNFromCoord (n + 1) board' coord
  in Set.foldl removeBoth b cs

clearOrthoNeighborsForAllSingletons :: Board -> Board
clearOrthoNeighborsForAllSingletons b =
  let allSingles = Set.filter (isSingleton b) allCoords
  in Set.foldl clearOrthoNeighborsForSingleton b allSingles

{- TODO: Adjacent-number "sees" stuff.

For example:

    3 |       |
 4    | 4     |
      |       |
---------------
    3 |
      |
      |
-------

If the 3 is in the top-left corner, it can see the 4 top-right, so that doesn't
work.  Likewise the 4 there would see the 3 bottom-left.  The only option left
is 4 top-right and 3 bottom-left.  How do we express that constraint so that
it's computable?  It's kind of like the "sees" stuff, if we include that we
"see" an item [and thus block it] if we're on the same square.  So if all the
local options for a 3 are "seen" by a 4 in the top-left, that can't be 4, and
vice-versa.  How to define "all the local options"?  Likely "all the
possibilties in a given unit" as elsewhere, although there may be some
cross-unit situations that we miss taking advantage of...not sure if that's
possible.
-}

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
      spacer = concat $ take lineLength $ repeat "-"
  in concat $ intersperse [spacer] allNumberRows

dumpBoard :: Board -> IO ()
dumpBoard b = do
  mapM_ putStrLn $ boardAsRows b
  putStrLn ""

onePass :: Board -> Board
onePass b =
  clearOrthoNeighborsForAllSingletons $
    clearOwnedCellForAllN $
    clearSeenForAllN $
    clearOutAllUnitsForAllN b

doUntilStable :: (Board -> Board) -> Board -> Board
doUntilStable f b =
  let helper board count =
        let board' = f board
            count' = countPossibilities board'
        in if count' < count
           then helper board' count'
           else board
  in helper b $ countPossibilities b

doYourBest :: Board -> Board
doYourBest b = doUntilStable onePass b

