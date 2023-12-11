-- IMPORTS
import Data.List
import Data.Char
import Data.Either
import Data.Maybe
import System.IO
import System.Directory (doesFileExist)


type ErrMsg = String
data Symbol = Sym Char | None deriving (Show, Eq) -- None is the blank symbol, it will not write to the tape
type Alphabet = [Symbol] 
type Tape = ([Symbol], Symbol, [Symbol]) -- A tape is a list of symbols, a head symbol, and symbols
data Direction = L | R | S deriving (Show, Eq) -- left, right, stay respectively
data State = Normal String | Accept | Reject deriving (Show, Eq)
type StateList = [State]
type Transition = (State, Symbol, Symbol, Direction, State)
type TransitionTable = [Transition]
data Specification = Spec Alphabet StateList TransitionTable deriving (Show, Eq)


helpMessage :: String
helpMessage = "Commands:\n\
    \h  - Show help\n\
    \l  - Load file\n\
    \t  - Test string\n\
    \tv - Test verbosely\n\
    \q  - Quit"

{- main :: IO ()
main = do
    let initialSpec = Spec [] [] [] -- Initial empty specification
    let loadedSpec = initialSpec    -- Define a variable to hold loaded specification

    putStr "> "
    command <- getLine
    case command of
        -- HELP
        "h" -> putStrLn helpMessage 
        "help" -> putStrLn helpMessage 
        
        -- LOAD
        "l" -> do
            putStrLn "Enter file path: "
            putStr "> "
            filePath <- getLine
            putStrLn ("Loading file: " ++ filePath)
            fileContent <- readFileToString filePath

            let loadedSpec = case fileContent of
                    Left errMsg -> initialSpec -- Assign initialSpec in case of error
                    Right contents ->
                        case parseSpecification contents of
                            Left errMsg -> initialSpec -- Assign initialSpec in case of parsing error
                            Right parsedSpec -> parsedSpec -- Assign parsed specification to newLoadedSpec

            case fileContent of
                Left errMsg -> putStrLn $ "Error loading file: " ++ errMsg
                Right _ -> do
                    putStrLn "Specification loaded successfully."


        -- TEST
        "t" -> do
            putStrLn "Enter input string: "
            putStr "> "
            input <- getLine

            case loadedSpec of 
                Spec [] [] [] -> putStrLn "No specification file loaded."
                spec -> do
                    let result = simulateTuringMachine spec input
                    putStrLn $ if result 
                        then "Accepted: " ++ input 
                        else "Rejected: " ++ input

        -- TEST VERBOSELY
        "tv" -> do
            putStrLn "Not implemented" 

        -- QUIT
        "q" -> do 
            putStrLn "Exiting."
            return()
        
        -- UNKNOWN COMMAND 
        unknown -> putStrLn ("Unknown command: '" ++ unknown ++ "'") 

    -- LOOP
    main -}
            
        
    
readFileToString :: FilePath -> IO (Either String String)
readFileToString filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            handle <- openFile filePath ReadMode
            contents <- hGetContents handle
            hClose handle
            return $ Right contents
        else return $ Left "File doesn't exist or is inaccessible."



{-
parseAlphabet: parses a csv of symbols converting them to an Alphabet
-}
parseAlphabet :: String -> Either Alphabet ErrMsg
parseAlphabet "" = Right ("No alphabet specified")
parseAlphabet s  
        | Just err <- find (\x -> length x > 1) stringList = Right ("Alphabet contains symbol with more than one character: '" ++ err ++ "'")
        | Just err <- find (not . isAlphaNum) charList = Right ("Alphabet contains non-alphanumeric symbol: '" ++ [err] ++ "'") 
        | otherwise = Left (map Sym charList)
        where 
            stringList = filter (not . null) (split ',' (trim s))
            charList = removeDuplicates (map head stringList)

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
        states = removeDuplicates (map (\x -> Normal x) (filter (not . null) (split ',' s)))
        validationResults = map (stateValid states) states

stateValid :: StateList -> State -> Either Bool ErrMsg
stateValid states (Normal s)
    | null s = Right "State name cannot be empty"
    | isUpper (head s) = Right ("State name must start with a lowercase character: '" ++ s ++ "'")
    | not (isAlpha (head s)) = Right ("State name must start with an alphabetic character: '" ++ s ++ "'")
    | any (not . isAlphaNum) s = Right ("State name must be alphanumeric: '" ++ s ++ "'")
    | s == "accept" = Right ("State name cannot be 'accept'")
    | s == "reject" = Right ("State name cannot be 'reject'")
    | otherwise = Left True


{-
parseTransitions: parses all transitions using parseTransition as a helper

parseTransition: parses a single transition
-}
parseTransitions :: [String] -> Alphabet -> StateList -> Either TransitionTable ErrMsg
parseTransitions [] _ _ = Right "No transitions found"
parseTransitions transitions alphabet stateList =
    let results = map (\x -> parseTransition x alphabet stateList) transitions
        errors = [e | Right e <- results]
        validTransitions = [t | Left t <- results]
    in if null errors
       then Left validTransitions
       else Right (head errors)




parseTransition :: String -> Alphabet -> StateList -> Either Transition ErrMsg
parseTransition str alphabet stateList 
    | length parts /= 5 = Right ("Invalid transition: '" ++ str ++ "'")
    | otherwise = case (state, readSyms, writeSym, dir, nextState) of
        (Left st, Left rs, Left ws, Left dr, Left ns) -> Left (st, rs, ws, dr, ns)
        (Right st, _, _, _, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ st)
        (_, Right rs, _, _, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ rs)
        (_, _, Right ws, _, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ ws)
        (_, _, _, Right dr, _) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ dr)
        (_, _, _, _, Right ns) -> Right ("Invalid transition: '" ++ str ++ "'. Reason: " ++ ns)
    where 
        parts = split '|' str
        state = lexState stateList (trim (parts !! 0)) 
        readSyms = lexSymbol alphabet (trim (parts !! 1)) 
        writeSym = lexSymbol alphabet (trim (parts !! 2)) 
        dir = lexDirection (trim (parts !! 3))
        nextState = lexState stateList (trim (parts !! 4))

lexSymbol :: Alphabet -> String -> Either Symbol ErrMsg
lexSymbol alphabet s | length s /= 1 = Right $ "Symbol must be one character: '" ++ s ++ "'"
lexSymbol alphabet "_" = Left None
lexSymbol alphabet s | Sym (head s) `elem` alphabet = Left (Sym (head s))
lexSymbol alphabet s = Right $ "Symbol not in alphabet: '" ++ [head s] ++ "'"

lexDirection :: String -> Either Direction ErrMsg
lexDirection ">" = Left R
lexDirection "<" = Left L
lexDirection "_" = Left S
lexDirection x = Right ("Invalid direction: '" ++ x ++ "'. Must be one of the following: '<', '>', '_'")

lexState :: StateList -> String -> Either State ErrMsg
lexState stateList "accept" = Left Accept
lexState stateList "reject" = Left Reject
lexState stateList s = if Normal s `elem` stateList
    then Left (Normal s)
    else Right ("State not found in statelist: '" ++ s ++ "'")


{-
parse: parses a specification file into a list of tokens
-}
parseSpecification :: String -> Either ErrMsg Specification
parseSpecification spec = do
    let (alphabetSection, stateSection, transitionLines) = extractSections (lines spec)
    alphabet <- case parseAlphabet alphabetSection of
        Left a -> return a
        Right errMsg -> Left errMsg
    states <- case parseStates stateSection of
        Left s -> return s
        Right errMsg -> Left errMsg
    transitions <- case parseTransitions transitionLines alphabet states of
        Left t -> return t
        Right errMsg -> Left errMsg
    return (Spec alphabet (Accept:Reject:states) transitions)


extractSections :: [String] -> (String, String, [String])
extractSections [] = ("", "", [])
extractSections (line:rest)
        | "alphabet:" `isPrefixOf` line = (trim $ dropWhile isSpace $ drop 9 line, statesSection, transitions)
        | "states:" `isPrefixOf` line = (alphabetSection, trim $ dropWhile isSpace $ drop 7 line, transitions)
        | "transitions:" `isPrefixOf` line = (alphabetSection, statesSection, rest)
        | otherwise = (alphabetSection, statesSection, rest)
    where
        (alphabetSection, statesSection, transitions) = extractSections rest

containsabc :: String
containsabc = 
    "alphabet:a,b,c\n" ++
    "states:q0,q1,q2\n" ++
    "transitions:\n" ++
    "q0 | a | _ | > | q1\n" ++ 
    "q0 | b | _ | > | q0\n" ++ 
    "q0 | c | _ | > | q0\n" ++ 
    "q0 | _ | _ | _ | reject\n" ++
    "q1 | b | _ | > | q2\n" ++
    "q1 | a | _ | > | q1\n" ++
    "q1 | c | _ | > | q0\n" ++
    "q1 | _ | _ | _ | reject\n" ++
    "q2 | c | _ | > | accept\n" ++
    "q2 | a | _ | > | q1\n" ++
    "q2 | b | _ | > | q0\n" ++
    "q2 | _ | _ | _ | reject"






main :: IO ()
main = do
    let specResult = parseSpecification containsabc
    case specResult of
        Left errMsg -> putStrLn $ "Error parsing specification: " ++ errMsg
        Right tmSpec -> do
            putStrLn "Specification loaded successfully."
            putStrLn "Enter input string: "
            input <- getLine
            let result = simulateTMWithLimit tmSpec (getInitialState tmSpec) (setInitialTape input (getAlphabetFromSpecification tmSpec)) 1000
            putStrLn $ outputResult result


{-
simulateTMWithLimit
-}

getInitialState :: Specification -> State
getInitialState (Spec _ (initialState:_) _) = initialState
getInitialState _ = error "Invalid Specification: No initial state found"

getAlphabetFromSpecification :: Specification -> Alphabet
getAlphabetFromSpecification (Spec alphabet _ _) = alphabet


simulateTMWithLimit :: Specification -> State -> Tape -> Int -> Either ErrMsg (State, Tape)
simulateTMWithLimit (Spec alphabet states transitions) currentState tape steps
  | steps <= 0 = Left "Exceeded maximum steps"
  | currentState == Accept || currentState == Reject = Right (currentState, tape)
  | otherwise =
      let (left, currentSymbol, right) = tape
      in case findTransition currentState currentSymbol transitions of
        Just (_, _, writeSymbol, direction, nextState) ->
          let newTape = moveTape direction (left, writeSymbol, right)
          in simulateTMWithLimit (Spec alphabet states transitions) nextState newTape (steps - 1)
        Nothing -> Left $ "Invalid transition for state '" ++ show currentState ++ "' and symbol '" ++ show currentSymbol ++ "'"



-- Utility function to find a transition in the TransitionTable
findTransition :: State -> Symbol -> TransitionTable -> Maybe Transition
findTransition state symbol = find (\(st, symbols, _, _, _) -> st == state && symbol == head [symbols])


moveTape :: Direction -> Tape -> Tape
moveTape L (ls, m, rs) = case ls of
  [] -> ([], None, m:rs)
  (l:ls':lss) -> ([l], ls', m:rs)
moveTape R (ls, m, rs) = case rs of
  [] -> (ls ++ [m], None, [])
  (r:rs') -> (ls ++ [m], r, rs')
moveTape S tape = tape


validateInput :: Alphabet -> Tape -> Bool
validateInput alphabet (left, currentSymbol, right) =
  all (`elem` alphabet) (left ++ [currentSymbol] ++ right)

setInitialTape :: String -> Alphabet -> Tape
setInitialTape input alphabet = ([], Sym (head input), map Sym (tail input))

displayTape :: Tape -> String
displayTape (left, Sym currentSymbol, right) = reverse (map symbolToChar left) ++ [symbolToChar (Sym currentSymbol)] ++ map symbolToChar right


symbolToChar :: Symbol -> Char
symbolToChar (Sym c) = c
symbolToChar None = '_' 

outputResult :: Either ErrMsg (State, Tape) -> String
outputResult (Left errMsg) = "Error: " ++ errMsg
outputResult (Right (state, tape)) = "Final state: " ++ show state ++ "\nFinal tape: " ++ displayTape tape




{-
General Helper Functions
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

-- Removes duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
