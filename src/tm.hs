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
       in Left (alp, lexStates grab : "accept" : "reject", tra) 
lexer s | take 14 s == "transitions:\n" 
    = let rest = drop 14 s
          (alp, sta, _) = lexer rest
       in Left (alp, sta, lexTransitions rest) 
lexer s = Right "Invalid specification file"
-}


{-
lexAlphabet: parses a csv of symbols converting them to an Alphabet
-}
lexAlphabet :: String -> Either Alphabet ErrMsg
lexAlphabet "" = Right ("No alphabet specified")
lexAlphabet s  
        | (any (\x -> length x > 1) stringList) = Right ("Alphabet contains symbol with more than one character" ) 
        | (any (not . isAlphaNum) charList) = Right ("Alphabet contains non-alphanumeric symbol" )
        | (duplicates charList) = Right ("Alphabet contains duplicate symbols")
        | otherwise = Left charList
        where 
            stringList = (split ',' s) 
            charList = map head stringList 



{-
lexStates: parses a csv of states
-}
lexStates :: String -> Either StateList ErrMsg
lexStates "" = Left []
lexStates s 
    | (isUpper (head name)) = Right ("State name must start with a lowercase character: " ++ name)
    | (not (isAlpha (head name))) = Right ("State name must start with an alphabetic character: " ++ name)
    | (any (not . isAlphaNum) name) = Right ("State name must be alphanumeric: " ++ name)
    | null rest = case name `elem` existingStates of
        True -> Right ("Duplicate state name found: " ++ name)
        False -> Left [name]
    | otherwise = case rest of
        ',':xs -> case lexStates xs of
                Left states -> if name `elem` states
                    then Right ("Duplicate state name found: " ++ name)
                    else Left (name : states)
                Right err -> Right err
        _ -> Right "Invalid state list format: missing comma separator"
    where 
        (name, rest) = break (== ',') s
        existingStates = case lexStates rest of
            Left states -> states
            Right _ -> []



{-
lexTransitions: parses all transitions using lexTransition as a helper
-}
{-
lexTransitions :: String -> TransitionTable
lexTransitions "" = []
lexTransitions s = let (current, rest) = span (\x -> x /= '\n') s
                    in lexTransition current : lexTransitions rest
-}


{-
lexTransition: parses a single transition 
data Transition = (State, [Symbol], Symbol, Direction, State)
-}
{-
lexTransition :: String -> (String, String, String, String, String)
lexTransition (x:xs) 
    | x == '|' = lexTransition xs
    | x == ' ' = lexTransition xs
    | otherwise = let 
        (state, four) = span (\x -> x /= '|') (x:xs)
        (states, three) = span (\x -> x /= '|') four 
        (write, two) = span (\x -> x /= '|') three
        (dir, one) = span (\x -> x /= '|') two
        (next, end) = span (\x -> x /= '|') one
        in (state, states, write, dir, next)
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