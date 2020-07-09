# core-hs

:construction: :construction: :construction: Working through S.L.Peyton Jones, D.Lester: Implementing Functional Languages. A Tutorial, 1992 (using Haskell).

The book gives a practical approach to understanding
implementations of **non-strict functional languages** using
**lazy graph reduction**.

The earlier book (SPJ 1987) covers similar material to this one,
but in a less practically oriented style. Part 1 of the latter,
which discusses how a high-level language can be translated into
a core language, is not covered here at all.

The `Core` language is designed to be as small as possible, so
that it is easy to implement, but still rich enough to allow
modern non-strict functional languages to be translated into it
without losing efficiency.

## Chapters

* Chapter 1 – The `Core` language
* Chapter 2 – Template instantiation
* Chapter 3 – G-machine
* Chapter 4 – Three instruction machine (TIM)
* Chapter 5 – Parallel G-machine
* Chapter 6 - Lambda lifter

Each implementation consists of two main parts:
__compiler__ and the __machine interpreter__.

The __compiler__ takes a `Core`-language program and translates
it into a form suitable for execution by the machine interpreter.

The __machine interpreter__ simulates the execution of the
compiled program and modeled as a __state transition system__.

## Notes

#### Chapter 1

#### Chapter 2

#### Chapter 3

#### Chapter 4

#### Chapter 5

#### Chapter 6
