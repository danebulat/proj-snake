# Snake Project 

## Overview

A terminal-based snake game written in Haskell.

Some Haskell concepts demonstrated in this application include:

- A monad transformer stack

  - `MaybeT`: Potentially stops processing a sequence of monadic actions
  - `ReaderT`: Implicitly passes around an immutable environment 
  - `StateT`: Contain mutable game data 
  - `WriterT`: Logs events that are later rendered in the TUI
  - `IO`: The base monad for performing I/O actions.
 
- Rendering a TUI with the `brick` library.

- Usage of the `lens` library for quickly accessing data in nested structures.

![Snake Game Demo](/doc/demo.gif)
