# instant
Additional libraries used:
- BNFC (much of the Main.hs is basically taken from the file autogenerated in there)
- hash-map for collection of the variables in the first compiler pass
- mtl for the State Monad (used for keeping the next available register in the LLVM compiler) and Reader Monad (JVM variables map)
- filepath and simple-cmd for all operations related to command-line.
