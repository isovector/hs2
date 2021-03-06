# Goals

## Prescriptive

1. Push Haskell to be a better language, this will involve removing some of the historical artifacts that are remnants of the stepping stones that got us to where we are today.
2. Don't let perfection be the enemy of the good. Prefer good solutions that
   improve everyday programmers' lives. This might look like removing of
   `String`, fix records, and adopt `-XExtraCommas`.
3. Backwards-compatibility should only be a concern up to supporting the boring
    90% of Hackage. Support enough of the ecosystem to be productive, but don't
    aim for 1:1 parity with GHC.
4. Make compiler choices that encourage end-users to write better programs.
   Enable `-Wall` by default. Enable `-XMonoLocalBinds` by default, forcing
   people to put type signatures on gnarly code.


## Product-Focused

1. Build a compiler that is explicitly *not* for research. Target industrial
   customers.
2. Start with tooling-support in mind. The MVP should launch with a
   language-server with equivalent features to HIE.
## Productive
1. Target fast compilation times. Full project builds should be no slower than GHC in the worst case, but we aim to do significantly better.
2. Feedback from the compiler during development should be quick and responsive. You should be able to know that you've introduced an error quickly, and that you've resolved an error quickly.

## Modular, Composable and Hackable

1. Separate the compiler into different packages for different passes --- one
   for the `parser`, one for the `typechecker`, another for the backend
   `codegen`.
2. Ensure an easy onboarding experience. Support building via the `stack` and
   `cabal` executables you already have installed.
3. Have an external API that is as easy as composing `typecheck . desugar .
   parse`.


## Simpler

1. Don't support language features that aggressively complicate the compiler;
   eg. no `-XTemplateHaskell` or `-XRebindableSyntax`. Likewise, desugar before
   typechecking.
2. Remove overlapping language features; only have one way to do things. Do we
   really need fundeps and type families? Stock deriving and GHC.Generics?
   Do-notation and list comprehensions?


## Invent Elsewhere

1. Greatly prefer existing software solutions to rolling our own. Target
   tree-sitter, GRIN and LLVM.
