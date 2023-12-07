 -- IMPORTS

import Data.List
import Data.Char
import Data.Either
import Data.Maybe
import Control.Monad

data Token = MetaData | Transitions | AlphaSec | StatesSec | InvalidInput String deriving (Show, Eq)
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

--type Specification = (Alphabet, StateList, TransitionTable) 
--type TuringMachine = (Specification, Tape)
data TuringMachine = TM Alphabet StateList TransitionTable deriving (Show)

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

mySplitOn :: String -> String -> [String]
mySplitOn _ [] = [""]
mySplitOn delimiter input@(x:xs)
    | delimiter `isPrefixOf` input = "" : mySplitOn delimiter (drop (length delimiter) input)
    | otherwise = (x : head rest) : tail rest
    where
        rest = mySplitOn delimiter xs
        isPrefixOf :: String -> String -> Bool
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (a:as) (b:bs)
            | a == b = isPrefixOf as bs
            | otherwise = False

-- Trim leading and trailing spaces from a string
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

-- Checks if a list contains duplicates
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = elem x xs || duplicates xs

-- Function to tokenize a line based on the provided format
tokenizeLine :: String -> Token
tokenizeLine line
    | "alphabet:" `isPrefixOf` line = AlphaSec
    | "states:" `isPrefixOf` line = StatesSec
    | "transitions:" `isPrefixOf` line = Transitions
    | otherwise = InvalidInput line

-- Function to tokenize the entire input
tokenizeInput :: String -> [Token]
tokenizeInput = map tokenizeLine . lines

{- parseAlphabet :: String -> Alphabet
parseAlphabet = map toSymbol . filter (/= "") . mySplitOn ","
  where
    toSymbol :: String -> Symbol
    toSymbol [c] = Sym c
    toSymbol _   = None -}

parseAlphabet :: String -> Either ErrMsg Alphabet
parseAlphabet input = do
    let symbols = filter (not . null) $ mySplitOn "," input
    checkDuplicates symbols
    checkSymbolLength symbols
    traverse toSymbol symbols

toSymbol :: String -> Either ErrMsg Symbol
toSymbol [c] = Right (Sym c)
toSymbol _   = Left "Invalid symbol format"

checkDuplicates :: Eq a => [a] -> Either ErrMsg ()
checkDuplicates lst =
    if duplicates lst
        then Left "Duplicate found"
        else Right ()

checkSymbolLength :: [String] -> Either ErrMsg ()
checkSymbolLength lst =
    if all (\s -> length s == 1) lst
        then Right ()
        else Left "Each symbol in the alphabet must be a single character"



{-parseStates :: String -> StateList
parseStates = map Normal . mySplitOn "," . last . words -}

parseStates :: String -> Either ErrMsg StateList
parseStates input = do
    let stateNames = mySplitOn "," $ last $ words input
    checkDuplicates stateNames
    traverse validateState stateNames

validateState :: String -> Either ErrMsg State
validateState state
    | state == "accept" = Right Accept
    | isValidState state = Right (Normal state)
    | otherwise = Left "Invalid state name"

isValidState :: String -> Bool
isValidState state
    | state == "accept" || state == "reject" = False  -- "accept" and "reject" are not allowed
    | otherwise =
        not (null state) &&
        isLower (head state) &&
        all (\c -> isAlphaNum c && not (isSpace c)) (tail state)
 


{- parseTransition :: String -> Transition
parseTransition line = case mySplitOn " | " line of
    [fromState, readSym, writeSym, direction, toState] ->
        let readSymbols = case readSym of
                "_" -> []
                [c] -> [Sym c]
                _   -> error "Invalid read symbol format"
            writeSymbol = case writeSym of
                "_" -> None
                [c] -> Sym c
                _   -> error "Invalid write symbol format"
            moveDirection = case direction of
                ">" -> R
                "<" -> L
                _   -> S
        in (Normal fromState, readSymbols, writeSymbol, moveDirection, Normal toState)
    _ -> error $ "Invalid transition format: " ++ line
 -}

parseTransition :: String -> Either ErrMsg Transition
parseTransition line = do
    let parts = mySplitOn " | " line
    when (length parts /= 5) $ Left "Invalid transition format"

    let [fromState, readSym, writeSym, direction, toState] = map trim parts

    fromState' <- validateState fromState
    readSymbols <- validateReadSymbols readSym
    writeSymbol <- validateWriteSymbol writeSym
    moveDirection <- validateDirection direction
    toState' <- validateState toState

    return (fromState', readSymbols, writeSymbol, moveDirection, toState')

validateReadSymbols :: String -> Either ErrMsg [Symbol]
validateReadSymbols readSym
    | readSym == "_" = Right []
    | all (\c -> isAlphaNum c || c == ',') readSym = traverse toSymbol (mySplitOn "," readSym)
    | otherwise = Left "Invalid read symbol format"


validateWriteSymbol :: String -> Either ErrMsg Symbol
validateWriteSymbol writeSym
    | writeSym == "_" = Right None
    | length writeSym == 1 && isAlphaNum (head writeSym) = toSymbol writeSym
    | otherwise = Left "Invalid write symbol format"



validateDirection :: String -> Either ErrMsg Direction
validateDirection dir
    | dir == ">" = Right R
    | dir == "<" = Right L
    | dir == "_" = Right S
    | otherwise = Left "Invalid move direction"



{- parseTransitions :: [String] -> TransitionTable
parseTransitions = map parseTransition
 -}

parseTransitions :: String -> Either ErrMsg TransitionTable
parseTransitions input = traverse parseTransition (lines input)

{- parseSpecification :: String -> Either ErrMsg TuringMachine
parseSpecification spec = do
    let sections = lines spec

    -- Parse alphabet
    let alphabetSection = takeWhile (\line -> not ("alphabet:" `isPrefixOf` line)) sections
    let remainingSectionsAfterAlphabet = drop (length alphabetSection) sections
    alphabet <- parseAlphabet (unlines alphabetSection)

    -- Parse states
    let statesSection = takeWhile (\line -> not ("states:" `isPrefixOf` line)) remainingSectionsAfterAlphabet
    let remainingSectionsAfterStates = drop (length statesSection) remainingSectionsAfterAlphabet
    states <- parseStates (unlines statesSection)

    -- Parse transitions
    let transitionLines = unlines $ dropWhile (\line -> not ("transitions:" `isPrefixOf` line)) remainingSectionsAfterStates
    transitions <- parseTransitions transitionLines

    return $ TM alphabet states transitions -}







mtrans :: String
mtrans = 
    "q0 | a | _ | > | q1 \n" ++ 
    "q0 | _ | _ | > | q0 \n" ++
    "q1 | b | _ | > | q2"

containsabc :: String
containsabc = 
    "alphabet:a,b,c\n" ++
    "states:q0,q1,q2,q3\n" ++
    "transitions:\n" ++
    "q0 | a | _ | > | q1 \n" ++ 
    "q0 | _ | _ | > | q0 \n" ++
    "q1 | b | _ | > | q2 \n" ++
    "q1 | _ | _ | > | q0 \n" ++
    "q2 | c | _ | > | q3 \n" ++
    "q2 | _ | _ | > | q0 \n" ++
    "q3 | _ | _ | > | accept \n" ++
    "q3 | a | _ | > | q1 \n" ++
    "q3 | b | _ | > | q2 \n" ++
    "q3 | c | _ | > | q3"

-- Check if a state is an accepting state
isAccepting :: State -> Bool
isAccepting Accept = True
isAccepting _      = False

-- Move the tape head in a given direction
-- Rewrite the moveHead function to correctly handle Tape type
moveHead :: Direction -> Tape -> Tape
moveHead L (l:ls, m, rs) = (init (l:ls), last (l:ls), m:rs)
moveHead R (ls, m, r:rs) = ((ls ++ [m]), r, rs)
moveHead _ tape           = tape

-- Get the transition for the current state and read symbol
getTransition :: State -> Symbol -> TransitionTable -> Maybe Transition
getTransition currentState currentSymbol transitions =
    find (\(fromState, readSymbols, _, _, _) -> fromState == currentState && elem currentSymbol readSymbols) transitions

-- Execute a single step of the Turing machine
-- Modify stepTM and simulate to handle the correct Tape type
stepTM :: TuringMachine -> Tape -> Maybe (TuringMachine, Tape)
stepTM (TM alphabet states transitions) tape@(left, current, right) =
    case current of
        None -> Nothing  -- Blank symbol, halt
        _    ->
            let currentState = head states
                transition = getTransition currentState current transitions
            in case transition of
                Just (fromState, _, writeSymbol, direction, toState) ->
                    let newTape = moveHead direction tape
                        newStateList = if isAccepting toState then [toState] else tail states ++ [toState]
                        newTM = TM alphabet newStateList transitions
                    in Just (newTM, newTape)
                Nothing -> Nothing  -- No valid transition found, halt
{- 
-- Correcting simulateTM function signature and implementation
simulateTM :: TuringMachine -> [Symbol] -> Int -> (TuringMachine, Tape)
simulateTM tm input steps = simulate tm (input, None, [])
  where
    simulate :: TuringMachine -> Tape -> (TuringMachine, Tape)
    simulate machine tape@(left, current, right)
      | steps <= 0 = (machine, tape)
      | otherwise =
        case stepTM machine tape of
          Just (newMachine, newTape) ->
            if isAccepting (head (getStateList newMachine))
              then (newMachine, newTape)
              else simulate newMachine newTape
                    -- Subtracting step count by 1 in each iteration
                    (steps - 1)
          Nothing -> (machine, tape)  -- Halt due to no valid transition
      where
        getStateList :: TuringMachine -> StateList
        getStateList (TM _ states _) = states -}




-- Helper function to convert a string to a list of symbols
stringToSymbols :: String -> [Symbol]
stringToSymbols = map Sym

{- main :: IO ()
main = do
    let specification = containsabc
        inputString = "abc"  -- Input string to simulate
        tm = parseSpecification specification
        inputSymbols = stringToSymbols inputString

    let (finalTM, finalTape) = simulateTM tm inputSymbols 1000
    putStrLn $ "Final Turing Machine state: " ++ show finalTM
    putStrLn $ "Final Tape: " ++ show finalTape -}