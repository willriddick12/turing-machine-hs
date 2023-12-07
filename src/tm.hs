-- IMPORTS
import Data.List
import Data.Char
import Data.Either

type ErrMsg = String

data Symbol = Sym Char | None deriving (Show, Eq) -- None is the blank symbol, it will not write to the tape
type Alphabet = [Symbol] 

-- A tape is a list of symbols, a head symbol, and symbols
type Tape = ([Symbol], Symbol, [Symbol]) 
data Direction = L | R | S deriving (Show, Eq) -- left, right, stay respectively

data State = Normal String | Accept | Reject deriving (Show, Eq)
type StateList = [State]

type Transition = (State, [Symbol], Symbol, Direction, State)
type TransitionTable = [Transition]

type Specification = (Alphabet, StateList, TransitionTable) 
type TuringMachine = (Specification, Tape)



{-
    GENERAL HELPER FUNCTIONS
-}
-- Splits a list on a delimiter
split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim lst =
    let (first, remainder) = span (/= delim) lst
     in first : case remainder of
        [] -> []
        (_:xs) -> split delim xs

-- Trim leading and trailing spaces from a string
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

-- Checks if a list contains duplicates
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = elem x xs || duplicates xs


{-
lexer: parses a specification file into a list of tokens
-}
{-
lexer :: String -> Either Specification ErrMsg
lexer s | take 9 s == "alphabet:" 
    = let (grab, rest) = span (\x -> x /= "\n") (drop 9 s) 
          (_, sta, tra) = lexer rest
       in Left (lexAlphabet grab, sta, tra) 
lexer s | take 7 s == "states:" 
    = let (grab, rest) = span (\x -> x /= "\n") (drop 9 s) 
          (alp, _, tra) = lexer rest 
       in Left (alp, parseStates grab, tra) 
lexer s | take 14 s == "transitions:\n" 
    = let rest = drop 14 s
          (alp, sta, _) = lexer rest
       in Left (alp, sta, parseTransitions rest) 
lexer s = Right "Invalid specification file"
-}



{-
parseAlphabet: parses a csv of symbols converting them to an Alphabet
-}
parseAlphabet :: String -> Either Alphabet ErrMsg
parseAlphabet "" = Right ("No alphabet specified")
parseAlphabet s  
        | Just err <- find (\x -> length x > 1) stringList = Right ("Alphabet contains symbol with more than one character: '" ++ err ++ "'")
        | Just err <- find (not . isAlphaNum) charList = Right ("Alphabet contains non-alphanumeric symbol: '" ++ [err] ++ "'") 
        | (duplicates charList) = Right "Alphabet contains duplicate symbols"
        | otherwise = Left (map Sym charList)
        where 
            stringList = filter (not . null) (split ',' s) 
            charList = map head stringList 

{-
parseStates: parses a csv of states

stateValid: checks if a state is valid
-}
parseStates :: String -> Either StateList ErrMsg
parseStates s =
    case find isRight validationResults of
        Nothing -> Left states  
        Just (Right errMsg) -> Right errMsg  
    where
        states = map (\x -> Normal x) (filter (not . null) (split ',' s))
        validationResults = map (stateValid states) states

stateValid :: StateList -> State -> Either Bool ErrMsg
stateValid states (Normal s)
    | null s = Right "State name cannot be empty"
    | s `elem` stateNamesWithoutCurrent = Right ("Duplicate state name found: '" ++ s ++ "'")
    | isUpper (head s) = Right ("State name must start with a lowercase character: '" ++ s ++ "'")
    | not (isAlpha (head s)) = Right ("State name must start with an alphabetic character: '" ++ s ++ "'")
    | any (not . isAlphaNum) s = Right ("State name must be alphanumeric: '" ++ s ++ "'")
    | s == "accept" = Right ("State name cannot be 'accept'")
    | s == "reject" = Right ("State name cannot be 'reject'")
    | otherwise = Left True
    where
        stateNames = map (\(Normal name) -> name) states
        stateNamesWithoutCurrent = filter (/= s) stateNames

        


{-
parseTransitions: parses all transitions using parseTransition as a helper

parseTransition: parses a single transition
-}
{-
parseTransitions :: String -> StateList -> Alphabet -> Either TransitionTable ErrMsg
parseTransitions "" _ _ = Right []  
parseTransitions s stateList alphabet =
    case parsedCurrent of
        Left transition -> case parseTransitions rest stateList alphabet of
            Left transitions -> Left (transition : transitions)
            Right errMsg -> Right errMsg
        Right errMsg -> Right errMsg 
    where
        (current, rest) = span (/= '\n') s
        parsedCurrent = parseTransition current stateList alphabet
-}

parseTransitions :: String -> StateList -> Alphabet -> Either TransitionTable ErrMsg
parseTransitions "" _ _ = Right []  
parseTransitions s stateList alphabet =
    case lines s of
        [] -> Right "No transitions found"
        transitions -> parseTransitionList transitions
    where
        parseTransitionList [] = Left []
        parseTransitionList (t:ts) =
            case parseTransition t stateList alphabet of
                Left transition ->
                    case parseTransitionList ts of
                        Left transitions -> Left (transition : transitions)
                        Right errMsg -> Right errMsg
                Right errMsg -> Right errMsg

test1 :: String
test1 = "q0 | a   | _ | > | q1 \n q0 | b,c | _ | > | q0 \n q0 | _   | _ | _ | reject \n"

parseTransition :: String -> StateList -> Alphabet -> Either Transition ErrMsg
parseTransition str stateList alphabet 
    | length parts /= 5 = Right ("Invalid transition: '" ++ str ++ "'")
    | otherwise = case (state, readSyms, writeSym, dir, nextState) of
        (Left st, Left rs, Left ws, Left d, Left ns) -> Left (st, rs, ws, d, ns)
        (Right st, _, _, _, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ st)
        (_, Right rs, _, _, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ rs)
        (_, _, Right ws, _, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ ws)
        (_, _, _, Right d, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ d)
        (_, _, _, _, Right ns) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ ns)
    where 
        parts = split '|' str
        state = lexState stateList (trim (parts !! 0)) 
        readSyms = lexSymbols alphabet (trim (parts !! 1)) 
        writeSym = lexSymbol alphabet (trim (parts !! 2)) 
        dir = lexDirection (trim (parts !! 3))
        nextState = lexState stateList (trim (parts !! 4))

lexSymbols :: Alphabet -> String -> Either [Symbol] ErrMsg
lexSymbols alphabet s =
  case find isRight symbols of
    Just err -> Right (fromRight err)
    Nothing -> Left (lefts symbols)
  where
    symbols =  map (lexSymbol alphabet) (filter (not . null) (split ',' s))
    isRight (Right _) = True
    isRight _ = False
    fromRight (Right x) = x

lexSymbol :: Alphabet -> String -> Either Symbol ErrMsg
lexSymbol alphabet s | length s /= 1 = Right $ "Symbol must be one character: '" ++ s ++ "'"
lexSymbol _ ['_'] = Left None
lexSymbol alphabet [s] | Sym s `elem` alphabet = Left (Sym s)
lexSymbol _ [s] = Right $ "Symbol not in alphabet: '" ++ [s] ++ "'"

lexDirection :: String -> Either Direction ErrMsg
lexDirection s = case s of
    ">" -> Left R
    "<" -> Left L
    "_" -> Left S
    x   -> Right ("Invalid direction: '" ++ x ++ "'. Must be one of the following: '<', '>', '_'")

lexState :: StateList -> String -> Either State ErrMsg
lexState stateList s = case s of
    "accept" -> Left Accept
    "reject" -> Left Reject
    state ->
        if Normal state `elem` stateList
            then Left (Normal state)
            else Right ("State not found: '" ++ state ++ "'")




{-
    simulateTMWithLimit
-}

{-
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

validateTransitionTable :: Specification -> Bool
validateTransitionTable (alphabet, states, transitions) =
  all (\s -> all (\a -> any (\(_, sym, _, _, _) -> s == Normal "" || a `elem` sym) transitions) alphabet) states

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



main :: IO ()
main = do
  let (alphabet, states, transitions) = specification
      inputTape = "aaaaaccbac" -- Input tape here
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
-}
{-
alphabet:a,b,c
states:q0,q1,q2
transitions:
q0 | a   | _ | > | q1
q0 | b,c | _ | > | q0
q0 | _   | _ | _ | reject

q1 | b | _ | > | q2
q1 | a | _ | > | q1
q1 | c | _ | > | q0
q1 | _ | _ | _ | reject

q2 | c | _ | > | accept
q2 | a | _ | > | q1
q2 | b | _ | > | q0
q2 | _ | _ | _ | rejectp
-}

--specification :: Specification
--specification = (['a', 'b', 'c'], [Normal "q0", Normal "q1", Normal "q2", Reject, Accept], [ (Normal "q0", ['a'], '_', R, Normal "q1"),(Normal "q0", ['b', 'c'], '_', R, Normal "q0"),(Normal "q0", ['_'], '_', S, Reject),(Normal "q1", ['b'], '_', R, Normal "q2"),(Normal "q1", ['a'], '_', R, Normal "q1"),(Normal "q1", ['c'], '_', R, Normal "q0"),(Normal "q1", ['_'], '_', S, Reject),(Normal "q2", ['c'], '_', R, Accept),(Normal "q2", ['a'], '_', R, Normal "q1"),(Normal "q2", ['b'], '_', R, Normal "q0"),(Normal "q2", ['_'], '_', S, Reject)])

