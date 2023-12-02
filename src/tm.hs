-- IMPORTS
import Data.List
import Data.Char

type Symbol = Char 
type Alphabet = [Symbol] 

-- A tape is a list of symbols, a head symbol, and symbols
type Tape = ([Symbol], Symbol, [Symbol]) 

data State = Normal String | Accept | Reject deriving (Show, Eq)
type StateList = [State]

data Direction = L | R | S -- left, right, stay respectively

type Transition = (State, [Symbol], Symbol, Direction, State)
type TransitionTable = [Transition]

type Specification = (Alphabet, StateList, TransitionTable) 
type TuringMachine = (Specification, Tape)

type ErrMsg = String


-- Utility function to find a transition in the TransitionTable
findTransition :: State -> Symbol -> TransitionTable -> Maybe Transition
findTransition state symbol = find (\(st, symbols, _, _, _) -> st == state && symbol `elem` symbols)

moveTape :: Direction -> Tape -> Tape
moveTape L (ls, m, rs) = case ls of
  [] -> ([], ' ', m:rs)
  (l:ls':lss) -> ([l], ls', m:rs)
moveTape R (ls, m, rs) = case rs of
  [] -> (ls ++ [m], ' ', [])
  (r:rs') -> (ls ++ [m], r, rs')
moveTape S tape = tape

-- Function to simulate the Turing machine
simulateTM :: TuringMachine -> State -> Either ErrMsg State
simulateTM ((alphabet, states, transitions), tape) currentState =
  let (left, currentSymbol, right) = tape
  in case findTransition currentState currentSymbol transitions of
    Just (_, _, writeSymbol, direction, nextState) ->
      let newTape = moveTape direction (left, writeSymbol, right)
      in simulateTM ((alphabet, states, transitions), newTape) nextState
    Nothing -> if currentState == Reject then Left "False'" else Right currentState

validateTransitionTable :: Specification -> Bool
validateTransitionTable (alphabet, states, transitions) =
  all (\s -> all (\a -> any (\(_, sym, _, _, _) -> s == Normal "" || a `elem` sym) transitions) alphabet) states


simulateTMWithLimit :: TuringMachine -> State -> Int -> Either ErrMsg (State, Tape)
simulateTMWithLimit tm@(spec, tape) currentState steps
  | steps <= 0 = Left "Exceeded maximum steps"
  | currentState == Accept || currentState == Reject = Right (currentState, tape)
  | otherwise =
      let (alphabet, states, transitions) = spec
          (left, currentSymbol, right) = tape
      in case findTransition currentState currentSymbol transitions of
        Just (_, _, writeSymbol, direction, nextState) ->
          let newTape = moveTape direction (left, writeSymbol, right)
          in simulateTMWithLimit (spec, newTape) nextState (steps - 1)
        Nothing -> Left "Invalid transition"


validateInput :: Alphabet -> Tape -> Bool
validateInput alphabet (left, currentSymbol, right) =
  all (`elem` alphabet) (left ++ [currentSymbol] ++ right)

setInitialTape :: String -> Alphabet -> Tape
setInitialTape input alphabet = ([], head input, tail input)

displayTape :: Tape -> String
displayTape (left, currentSymbol, right) = reverse left ++ [currentSymbol] ++ right


outputResult :: Either ErrMsg (State, Tape) -> String
outputResult (Left errMsg) = "Error: " ++ errMsg
outputResult (Right (state, tape)) = "Final state: " ++ show state ++ "\nFinal tape: " ++ displayTape tape

specification :: Specification
specification = (['a', 'b', 'c'], [Normal "q0", Normal "q1", Normal "q2", Reject, Accept], [ (Normal "q0", ['a'], '_', R, Normal "q1"),(Normal "q0", ['b', 'c'], '_', R, Normal "q0"),(Normal "q0", ['_'], '_', S, Reject),(Normal "q1", ['b'], '_', R, Normal "q2"),(Normal "q1", ['a'], '_', R, Normal "q1"),(Normal "q1", ['c'], '_', R, Normal "q0"),(Normal "q1", ['_'], '_', S, Reject),(Normal "q2", ['c'], '_', R, Accept),(Normal "q2", ['a'], '_', R, Normal "q1"),(Normal "q2", ['b'], '_', R, Normal "q0"),(Normal "q2", ['_'], '_', S, Reject)])

main :: IO ()
main = do
  let (alphabet, states, transitions) = specification
      inputTape = "aaaaaccabcbac" -- Input tape here
      initialTape = setInitialTape inputTape alphabet
      tm = (specification, initialTape)
      initialState = Normal "q0"
      maxSteps = 1000

  if not (validateTransitionTable specification)
    then putStrLn "Transition table is incomplete"
    else if not (validateInput alphabet initialTape)
      then putStrLn "Input tape contains symbols not in the alphabet"
      else case simulateTMWithLimit tm initialState maxSteps of
        Left errMsg -> putStrLn $ "Error: " ++ errMsg
        Right (finalState, finalTape) -> putStrLn $ outputResult (Right (finalState, finalTape))
