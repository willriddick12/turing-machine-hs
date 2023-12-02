-- IMPORTS
import Data.List
import Data.Char
import Data.Either

type ErrMsg = String

type Symbol = Char 
type Alphabet = [Symbol] 

data State = Normal String | Accept | Reject 
    deriving (Eq, Show)
type StateList = [State]

-- A tape is a list of symbols, a head symbol, and symbols
type Tape = ([Symbol], Symbol, [Symbol]) 
data Direction = L | R | S -- left, right, stay respectively

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

-- Checks if a list contains duplicates
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = elem x xs || duplicates xs

-- Trim leading and trailing spaces from a string
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')


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
        | (any (\x -> length x > 1) stringList) = Right ("Alphabet contains symbol with more than one character" ) 
        | (any (not . isAlphaNum) charList) = Right ("Alphabet contains non-alphanumeric symbol" )
        | (duplicates charList) = Right ("Alphabet contains duplicate symbols")
        | otherwise = Left charList
        where 
            stringList = (split ',' s) 
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
parseTransitions :: String -> TransitionTable
parseTransitions "" = []
parseTransitions s = let (current, rest) = span (\x -> x /= '\n') s
                    in parseTransition current : lexTransitions rest
-}

{-
parseTransition :: String -> StateList -> Alphabet -> Either Transition ErrMsg
parseTransition s stateList alphabet
    | (not (stateExists curState stateList)) = Right ("State: '" ++ curState ++ "' does not exist")
    | (not (stateExists nextState stateList)) = Right ("State: '" ++ nextState ++ "' does not exist")
    | (not (symbolExists writeSym alphabet)) = Right ("Symbol: '" ++ writeSym ++ "' does not exist")
    | not (all (`symbolExists` alphabet) readSymList) = Right ("One or more symbols in list do not exist in the alphabet")
    | Right errMsg <- readSymList = Right errMsg
    | Right errMsg <- lexDir = Right errMsg
    | otherwise = Left (curState, readSymList, writeSym, lexDir, nextState)
    where
        [curState, readSym, writeSym, dir, nextState] = map (trim . dropWhileEnd isSpace) $ wordsWhen (== '|') s
        trim = f . f where f = reverse . dropWhile isSpace
        readSymList = parseAlphabet readSym
        lexDir = lexDirection dir
        -}



lexDirection :: String -> Either Direction ErrMsg
lexDirection s = case s of
    ">" -> Left R
    "<" -> Left L
    "_" -> Left S
    x   -> Right ("Invalid direction: '" ++ x ++ "'. Must be one of: '<', '>', '_'")

lexState :: String -> StateList -> Either State ErrMsg
lexState s stateList = case s of
    "accept" -> Left Accept
    "reject" -> Left Reject
    state ->
        if Normal state `elem` stateList
            then Left (Normal state)
            else Right ("State not found: '" ++ state ++ "'")

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

{-
main :: IO ()
main = do
    putStrLn "Input a specification file: "
    inp <- getLine
        lexer inp

        main
-}