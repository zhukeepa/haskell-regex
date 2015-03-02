module Regex.Build (matchRegex) where

import Regex.Types
import Data.Set as S

-- data Rx = Empty | Single Char | Star Rx | Union Rx Rx | Concat Rx Rx deriving (Show)

type State = Int 
data Transition = Transition Char | EmptyTrans deriving (Eq)
data Fsm = Fsm { states     :: [State], 
                 starts     :: Set State, -- must be closed under empty transitions
                 ends       :: Set State, 
                 trans      :: Transition -> State -> Set State -- all nodes must include empty trans with self
               }

applyCharTrans :: Set State -> (State -> Set State) -> Set State
applyCharTrans s trans = unions $ toList $ S.map trans s

-- Must iterate until set of states says the same. 
applyEmptyTrans :: Set State -> (State -> Set State) -> Set State
applyEmptyTrans s trans
    | size s < size w = applyEmptyTrans w trans 
    | otherwise       = s -- done when no longer increasing in size
    where w = applyCharTrans s trans

-- Given a transition function, go from one state to the next given a char. 
step :: (Transition -> State -> Set State) -> Set State -> Char -> Set State
step trans s c = applyEmptyTrans (applyCharTrans s (trans $ Transition c)) (trans EmptyTrans)

runFsm :: Fsm -> String -> Bool
runFsm fsm str = includesEndState $ Prelude.foldl (step $ trans fsm) (starts fsm) str
    where includesEndState s = not $ S.null $ intersection (ends fsm) s


-- Returns an equivalent fsm where all the states, interpreted as integers, get increased by
-- the same number. The purpose is to ensure that states are distinct when merging two different
-- fsm's. 
shiftFsm :: Int -> Fsm -> Fsm
shiftFsm l (Fsm { states = states', starts = starts', ends = ends', trans = trans' }) = 
    Fsm { states = Prelude.map (+ l) states', 
          starts = S.map (+ l) starts',
          ends = S.map (+ l) ends', 
          trans = (\c x -> S.map (+ l) (trans' c (x - l))) }

buildFsm :: Rx -> Fsm
buildFsm Empty = Fsm { states = [0,1], 
                       starts = singleton 0, 
                       ends = singleton 0, 
                       trans = emptyTrans }
    where emptyTrans EmptyTrans x = singleton x
          emptyTrans _  _         = singleton 1

buildFsm (Single c) = Fsm { states = [0,1,2], 
                            starts = singleton 0, 
                            ends = singleton 1, 
                            trans = charTrans }
    where charTrans EmptyTrans x = singleton x
          charTrans x s
              | (x,s) == (Transition c,0) = singleton 1
              | otherwise = singleton 2

buildFsm (Star rx) = Fsm { states = states', starts = starts', ends = ends', trans = trans' }
    where fsm = buildFsm rx
          states' = states fsm
          starts' = starts fsm
          ends'   = starts'
          trans' c s
              -- if you're at an (old) end state, add empty transitions to start states. 
              | (c == EmptyTrans) && (s `member` ends fsm) = union starts' ((trans fsm) EmptyTrans s)
              | otherwise                                  = (trans fsm) c s

buildFsm (Union rx1 rx2) = Fsm { states = states', starts = starts', ends = ends', trans = trans' }
    where fsm1 = buildFsm rx1
          fsm2 = shiftFsm (length $ states fsm1) (buildFsm rx2) -- turns out all the states are always consecutive integers, so this works
          states' = (states fsm1) ++ (states fsm2)
          starts' = union (starts fsm1) (starts fsm2)
          ends'   = union (ends fsm1) (ends fsm2)
          trans' c s
              -- gluing together the two transition functions
              | s `elem` (states fsm1) = (trans fsm1) c s
              | otherwise              = (trans fsm2) c s

buildFsm (Concat rx1 rx2) = Fsm { states = states', starts = starts', ends = ends', trans = trans' }
    where fsm1 = buildFsm rx1
          fsm2 = shiftFsm (length $ states fsm1) (buildFsm rx2)
          states' = (states fsm1) ++ (states fsm2)
          starts' = starts fsm1
          ends'   = ends fsm2
          trans' c s
              -- add empty transition from ends1 to starts2
              | (c == EmptyTrans) && (s `member` ends fsm1) = union (starts fsm2) ((trans fsm1) c s)
              | s `elem` (states fsm1) = (trans fsm1) c s
              | otherwise              = (trans fsm2) c s

matchRegex :: Rx -> String -> Bool
matchRegex = runFsm . buildFsm

-- rx = Concat (Star (Union (Single 'A') (Single 'B'))) (Star (Union (Single 'C') (Single 'D')))
-- main = print $ matchRegex rx "BABABABABACDCDCDCD"