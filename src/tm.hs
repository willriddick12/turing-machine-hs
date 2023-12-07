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
    {-GENERAL HELPER FUNCTIONS
-}
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

parseAlphabet :: String -> Alphabet
parseAlphabet = map toSymbol . filter (/= "") . mySplitOn ","
  where
    toSymbol :: String -> Symbol
    toSymbol [c] = Sym c
    toSymbol _   = None 




parseStates :: String -> StateList
parseStates = map Normal . mySplitOn "," . last . words


parseTransition :: String -> Transition
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
            trimmedFromState = trim fromState
            trimmedToState = trim toState
        in (Normal trimmedFromState, readSymbols, writeSymbol, moveDirection, Normal trimmedToState)
    _ -> error $ "Invalid transition format: " ++ line



parseTransitions :: [String] -> TransitionTable
parseTransitions = map parseTransition



parseSpecification :: String -> TuringMachine
parseSpecification spec =
    let sections = lines spec
        (alphabetSection, stateSection, transitionLines) = extractSections sections
        alphabet = parseAlphabet alphabetSection
        states = parseStates stateSection
        transitions = parseTransitions transitionLines
    in TM alphabet states transitions

extractSections :: [String] -> (String, String, [String])
extractSections [] = ("", "", [])
extractSections (line:rest)
    | "alphabet:" `isPrefixOf` line = (trim $ dropWhile isSpace $ drop 9 line, statesSection, transitions)
    | "states:" `isPrefixOf` line = (alphabetSection, trim $ dropWhile isSpace $ drop 7 line, transitions)
    | "transitions:" `isPrefixOf` line = (alphabetSection, statesSection, rest)
    | otherwise = (alphabetSection, statesSection, rest)
  where
    (alphabetSection, statesSection, transitions) = extractSections rest


{-
    TM [Sym 'a',Sym 'b',Sym 'c'] [Normal "q0",Normal "q1",Normal "q2",Normal "q3"] 
    [(Normal "q0",[Sym 'a'],None,R,Normal "q1 "),
    (Normal "q0",[],None,R,Normal "q0 "),
    (Normal "q1",[Sym 'b'],None,R,Normal "q2 "),
    (Normal "q1",[],None,R,Normal "q0 "),
    (Normal "q2",[Sym 'c'],None,R,Normal "q3 "),
    (Normal "q2",[],None,R,Normal "q0 "),
    (Normal "q3",[],None,R,Normal "accept "),
    (Normal "q3",[Sym 'a'],None,R,Normal "q1 "),
    (Normal "q3",[Sym 'b'],None,R,Normal "q2 "),(Normal "q3",[Sym 'c'],None,R,Normal "q3")]
-}


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
