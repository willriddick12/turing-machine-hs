

data Symbol = Char | None
type Alphabet = [Symbol]

-- A tape is a list of symbols, a head symbol, and symbols
type Tape = ([Symbol], Symbol, [Symbol]) 

type State = String
data StateList = [State]

data Direction = L | R | None
data Transition = (State, [Symbol], Symbol, Direction, State)
type TransitionTable = [Transition]

data Specification = (Alphabet, StateList, TransitionTable) | Error String
data TuringMachine = (Specification, Tape)

{-
    lexer: parses a specification file into a list of tokens
-}
lexer :: String -> Specification 
lexer s | take 9 s == "alphabet:" 
    = let (grab, rest) = span (\x -> x /= "\n") (drop 9 s) 
          (_, sta, tra) = lexer rest
       in (lexAlphabet grab, sta, tra) 
lexer s | take 7 s == "states:" 
    = let (grab, rest) = span (\x -> x /= "\n") (drop 9 s) 
          (alp, _, tra) = lexer rest 
       in (alp, lexState grab, tra) 
lexer s | take 14 s == "transitions:\n" 
    = let (alp, sta, _) = lexer rest
       in (alp, sta, lexTransitions rest) 



{-
lexAlphabet: parses a csv of symbols converting them to an Alphabet
-}
lexAlphabet :: String -> Alphabet 
lexAlphabet "" = []
lexAlphabet (x:xs) | x == ',' = lexAlphabet xs
                   | otherwise = x : lexAlphabet xs

{-
lexStates: parses a csv of states
-}
lexStates :: String -> [State]
lexStates "" = []
lexAlphabet (x:xs) = let (name,rest) = span (\x -> x /= ',') (x:xs) 
                      in name : lexAlphabet rest

{-
lexTransitions: parses transitions
-}
lexTransitions :: String -> TransitionTable


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

main :: IO ()
main = do
    putStrLn "Input a specification file: "
    inp <- getLine
        lexer inp

        main