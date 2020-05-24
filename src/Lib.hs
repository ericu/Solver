module Lib
    ( test
    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (partition)

test :: IO ()
test = do
  dumpBoard initialBoard

data Coord = Coord { col :: Int, row :: Int } deriving (Show, Eq, Ord)
type Board = Map Coord (Set Int)
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
clearSolved b = foldl clearSolvedForN b [0..8]

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
allRows = Set.fromList [Set.fromList [Coord c r | c <- [0..8]] | r <- [0..8]]
allCols :: Set Unit
allCols = Set.fromList [Set.fromList [Coord c r | r <- [0..8]] | c <- [0..8]]
allBoxes :: Set Unit
allBoxes = Set.empty -- TODO

allUnits :: Set Unit
allUnits = allRows `Set.union` allCols `Set.union` allBoxes

allCoords = Set.fromList [Coord c r | c <- [0..8], r <- [0..8]]

initialBoard :: Board
initialBoard =
  let blank = Set.fromList [0..8]
  in Map.fromList $ zip (Set.toList allCoords) $ repeat blank

-- TODO: Pretty board display
dumpBoard :: Board -> IO ()
dumpBoard b = do
  let kvs = map (\c -> (show c) ++ ": " ++ (show $ b Map.! c))
                (Set.toList allCoords)
  mapM_ putStrLn kvs
  
