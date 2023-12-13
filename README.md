# Turing Machine in Haskell

A Turing Machine implemented in Haskell by Bryce Gerdeman, Will Riddick, and Noah Salam.

A Turing Machine is a hypothetical computational device that consists of a head and a tape. The head can move left and right along the tape, reading and writing symbols based on a set of specifications.

## Running the Project

To run the project:

1. Load the `tm.hs` file into GHCi.
2. Run the `main` function.
3. In the CLI, select 'h' to view available commands.
4. To load a file, select 'l' and enter the file path of the specification.
5. The project includes extensive error checking for proper formatting in specification files, helping users with potential issues.


## Specification Format:

Specification files are basic _.txt_ files following the format outlined below:

- Symbols: Symbols are single alphanumeric characters.
- States: States must start with a lowercase letter and may only contain alphanumeric characters.
- Transitions: Transitions are defined by a current state, a read symbol, a write symbol, a direction, and a next state. Transitions may be listed in any order. 
    - Symbols or states mentioned in a transition must be defined in the specification file. 
    - Directions must be specified as '>' (right), '<' (left), or '\_' (stay). Use '_' to denote an empty symbol ('any symbol' in read or 'no symbol' in write).
    - Halt states include 'accept' and 'reject'. Add either of these into the next state of a transition to determine whether a string is accepted or rejected. These states are implied and do not need to be listed in the 'states:' section of the specification file.

```
alphabet:<csv symbols>
states:<csv of states>
transitions:
<state> | <read symbol> | <write symbol> | <direction> | <next state>
<state> | <read symbol> | <write symbol> | <direction> | <next state>
...
```

## Example Specification
Below is an example specification file which accepts strings containing the substring 'abc'.
```
alphabet:a,b,c
states:q0,q1,q2
transitions:
q0 | a | _ | > | q1
q0 | b | _ | > | q0
q0 | c | _ | > | q0
q0 | _ | _ | _ | reject
q1 | b | _ | > | q2
q1 | a | _ | > | q1
q1 | c | _ | > | q0
q1 | _ | _ | _ | reject
q2 | c | _ | > | accept
q2 | a | _ | > | q1
q2 | b | _ | > | q0
q2 | _ | _ | _ | reject
```

## Testing Strings

Once a specification file is loaded:

- Select 't' to test a string.
- The implementation returns "Accept" or "Reject" based on whether the provided specification file accepts the string.
- You can test strings without reloading the specification file.
- To overwrite the currently loaded specification file, choose the load option again.
- To exit the CLI interface, select 'q'.