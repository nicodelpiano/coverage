## coverage

### An exhaustivity checking library

Copyright 2015, Nicolas Del Piano <ndel314@gmail.com>.

This package provides a tool that performs exhaustivity and redundancy checking over custom pattern matching definitions.

Installation:

    cabal install coverage

You can see examples of usage in the `examples` directory and read the Haddock documentation.

#### Tests

For running tests just execute:

    cabal test

or, if you have `hspec-discover` (inside the `tests` directory run):

    runhaskell Spec.hs
