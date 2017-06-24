# NFautomaton

Nondeterministic finite automaton


## Auto

1. **accepts aut w** check if aut accepts string w
2. **emptyA** NFA that accepts only empty language
3. **epsA** NFA that accepts only empty string
4. **symA c** NFA that accepts only one element string {c}
5. **leftA aut** NFA that accepts only the same language as aut
6. **sumA aut1 aut2** sum
7. **thenA aut1 aut2** concatenation
8. **fromLists** i **toLists** change representation of automaton


## RunAuto

**RunAuto n** read description of automaton and example string from file n and check if given automaton accepts given string