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
--  dumpBoard $ clearSeenForN (clearOutAllUnitsForN initialBoard 3) 3
--  dumpBoard $ clearSeenForNFromUnit 3 (clearOutAllUnitsForN initialBoard 3)
--                                    testBox
--  dumpBoard $ clearOutAllUnitsForN initialBoard 3
--  dumpBoard $ (clearSeenForAllN) $
--              (clearOutAllUnitsForAllN) initialBoard
  dumpBoard $ (doUntilStable clearSeenForAllN) $
              (doUntilStable clearOutAllUnitsForAllN) initialBoard

data Offset = Offset { dc :: Int, dr :: Int } deriving (Show, Eq, Ord)
data Coord = Coord { col :: Int, row :: Int } deriving (Show, Eq, Ord)
type State = Set Int
numberRange = [1..9]
coordRange = [0..8]
type Board = Map Coord State
type Unit = Set Coord

countPossibilities :: Board -> Int
countPossibilities b =
  let addSet soFar s = soFar + Set.size s
  in Map.foldl addSet 0 b

allPossibleCoordsFor :: Board -> Int -> Set Coord
allPossibleCoordsFor b n = allPossibleCoordsInUFor b allCoords n

allPossibleCoordsInUFor :: Board -> Unit -> Int -> Set Coord
allPossibleCoordsInUFor b u n =
  Set.filter (\c -> n `Set.member` (b Map.! c)) u

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

clearOwnedCellForN :: Board -> Int -> Board
clearOwnedCellForN b n = Set.foldl (clearOwnedCellInUnitForN n) b allUnits

clearOwnedCellForAllN :: Board -> Board
clearOwnedCellForAllN b = foldl clearOwnedCellForN b numberRange

doUntilStable :: (Board -> Board) -> Board -> Board
doUntilStable f b =
  let helper board count =
        let board' = f board
            count' = countPossibilities board'
        in if count' < count
           then helper board' count'
           else board
  in helper b $ countPossibilities b

removeNFromCoord :: Int -> Board -> Coord -> Board
removeNFromCoord n b c =
  let state = b Map.! c
      state' = Set.delete n state
  in Map.insert c state' b

clearOutUnitForN :: Int -> Board -> Unit -> Board
clearOutUnitForN n b u =
  let isSingleN coord = b Map.! coord == Set.singleton n
      listOfSingles = Set.toList $ Set.filter isSingleN u
  in case listOfSingles of
       [c] -> let otherCoords = u `Set.difference` (Set.singleton c)
              in Set.foldl (removeNFromCoord n) b otherCoords
       a:b:c -> error "Unexpected Conflict"
       _ -> b
       

clearOutAllUnitsForN :: Board -> Int -> Board
clearOutAllUnitsForN b n = Set.foldl (clearOutUnitForN n) b allUnits

clearOutAllUnitsForAllN :: Board -> Board
clearOutAllUnitsForAllN b = foldl clearOutAllUnitsForN b numberRange

{-
countSolved :: Board -> Int
countSolved b =
  let solved = filter (\s -> Set.size s == 1) (Map.elems b)
  in length solved
  -}

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

setValue :: Coord -> Int -> Board -> Board
setValue c n b =
  Map.insert c (Set.singleton n) b

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
{-
Exterior check:
  For each number N
    For each unit U
      let S = all the possible places in U that N can be
      let L = all the locations NOT in U that N can be
      -- So filter all sets that have N as possible, then partition into S,L.
      clear N from $ filter (all S see) l

-}
clearSeenForNFromUnit :: Int -> Board -> Unit -> Board
clearSeenForNFromUnit n b u =
  let isNPossibleAtCoord c = Set.member n $ b Map.! c
      allNLocations = Set.filter isNPossibleAtCoord allCoords
      nInUnit = allNLocations `Set.intersection` u
      nOutOfUnit = allNLocations `Set.difference` nInUnit
      seenFromAllInUnit c = all (sees c) nInUnit
      f board c = if seenFromAllInUnit c
                  then traceShow (c, "taken out by unit", u) $
                                 removeNFromCoord n board c
                  else board
  in foldl f b nOutOfUnit

clearSeenForN :: Board -> Int -> Board
clearSeenForN b n = Set.foldl (clearSeenForNFromUnit n) b allUnits

clearSeenForAllN :: Board -> Board
clearSeenForAllN b = foldl clearSeenForN b numberRange

-- TODO: Adjacent-number elimination
