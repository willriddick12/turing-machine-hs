-- IMPORTS
import Data.List
import Data.Char

type Symbol = Char 
type Alphabet = [Symbol] 

-- A tape is a list of symbols, a head symbol, and symbols
type Tape = ([Symbol], Symbol, [Symbol]) 

type State = String
type StateList = [State]

data Direction = L | R | S -- left, right, stay respectively
type Transition = (State, [Symbol], Symbol, Direction, State)
type TransitionTable = [Transition]

type Specification = (Alphabet, StateList, TransitionTable) 
type TuringMachine = (Specification, Tape)

type ErrMsg = String

let specification1 = (['a','b','c'], ["q0","q1","q2"], [("q0", ['a'], '_', R, "q1"), ("q0", ['b','c'], '_', R, "q0"), ("q0", ['_'], '_', S, "reject"), ("q1", ['b'], '_', R, "q2"), ("q1", ['a'], '_', R, "q1"), ("q1", ['c'], '_', R, "q0"), ("q1", ['_'], '_', S, "reject"), ("q2", ['c'], '_', R, "accept"), ("q2", ['a'], '_', R, "q1"), ("q2", ['b'], '_', R, "q0"), ("q2", ['_'], '_', S, "reject")

{-
HELPER FUNCTIONS
-}
split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim lst =
    let (first, remainder) = span (/= delim) lst
     in first : case remainder of
        [] -> []
        (_:xs) -> split delim xs

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
       in Left (alp, parseStates grab : "accept" : "reject", tra) 
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
-}
parseStates :: String -> Either StateList ErrMsg
parseStates "" = Left []
parseStates s 
    | (isUpper (head name)) = Right ("State name must start with a lowercase character: " ++ name)
    | (not (isAlpha (head name))) = Right ("State name must start with an alphabetic character: " ++ name)
    | (any (not . isAlphaNum) name) = Right ("State name must be alphanumeric: " ++ name)
    | null rest = case name `elem` existingStates of
        True -> Right ("Duplicate state name found: " ++ name)
        False -> Left [name]
    | otherwise = case rest of
        ',':xs -> case parseStates xs of
                Left states -> if name `elem` states
                    then Right ("Duplicate state name found: " ++ name)
                    else Left (name : states)
                Right err -> Right err
        _ -> Right "Invalid state list format: missing comma separator"
    where 
        (name, rest) = break (== ',') s
        existingStates = case parseStates rest of
            Left states -> states
            Right _ -> []



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


parseTransition :: String -> StateList -> Alphabet -> Either Transition ErrMsg
parseTransition s 
    | (not (stateExists curState)) = Right ("State: '" ++ curState ++ "' does not exist")
    | (not (stateExists nexState)) = Right ("State: '" ++ nexState ++ "' does not exist")
    | (not (symbolExists readSym)) = Right ("Symbol: '" ++ readSym ++ "' does not exist")
    | (any (symbolExists (lexAlphabet headSym))) = Right ("Symbol in list: '" ++ headSym ++ "' does not exist")
    | otherwise = Left (curState, lexAlphabet headSym, readSym, lexDirection dir, nexState)
    where
        [curState, headSym, readSym, dir, nexState] = map (trim . dropWhileEnd isSpace) $ wordsWhen (== '|') s
        trim = f . f where f = reverse . dropWhile isSpace

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

stateExists :: State -> StateList -> Bool
stateExists state stateList = elem state stateList

symbolExists :: Symbol -> Alphabet -> Bool
symbolExists sym alph = elem sym alph

lexDirection :: String -> Either Direction ErrMsg
lexDirection s = case s of
    ">" -> Left R
    "<" -> Left L
    "_" -> Left S
    x   -> Right ("Invalid direction: '" ++ x ++ "'. Must be one of: '<', '>', '_'")


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
q2 | _ | _ | _ | reject
-}

{-
main :: IO ()
main = do
    putStrLn "Input a specification file: "
    inp <- getLine
        lexer inp

        main
-}