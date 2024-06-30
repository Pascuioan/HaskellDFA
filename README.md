# HaskellDFA
This is a program that takes a DFA input file and generates a Haskell program that can run the DFA for an input string.

### Usage
Use `git clone https://github.com/Pascuioan/HaskellDFA.git` to clone the repository.
```
make generator # builds the converter executable
./Genrator.exe <input_file> # to generate the Haskell program based on the input file
make # builds the output into an executable
./dfa.exe <input> # run the DFA for an input string
```
### Input file
The input file must contain thee sections: `states`, `alphabet` and `transitions`, each containg a header specifying the contents. The sections must be separated by a row containing a single `#`.

The states must each be on a separate line, with the first one being the start state. To mark a state as an accepting state, place a single `!` at the end of the line.

Each symbol of the alphabet must be written on a separate line.

The transitions should have the format: `state symbol state` and be written on separate lines.

### Example 
Let's take a DFA that only accepts strings containing any number of 1s.
```
states
a!
fail
#
alphabet
0
1
#
transitions
a 1 a
a 0 fail
fail 0 fail
fail 1 fail
```

After building the DFA, we can run some tests:
```
>./dfa.exe 111
Accepted
>./dfa.exe 10
Failure
>./dfa.exe
Accepted
```

### Notes
- To run the DFA with the empty string, just run the executable without any arguments.
- The program doesn't automatically fail when a transition is not defined, so you should include rules for all possibilities of states and symbols.
- The program can only recognize symbols in the alphabet, so an input containing an undefined symbol will generate an error.
- The alphabet must be composed only of characters and cannot contain `#` or ` `(space).
