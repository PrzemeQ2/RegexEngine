# RegexEngine
A regular expression engine built from scratch in Haskell: Parser → NFA → DFA → Matcher.

## Overview
This project implements a regular expression engine end to end.
It takes a regex as a string and builds an AST from it.
The AST is then compiled into a nondeterministic finite automaton (NFA) using Thompson's construction.
The NFA can optionally be transformed into a DFA via the subset construction.
Finally, the matcher simulates the automaton on the input and returns a boolean indicating whether the regex matches the whole input string. Together, this illustrates the classical correspondence between regular expressions, NFAs, and DFAs.

## Build & Testing

## Architecture

## Usage Examples

## Grammar

## Tests