# Operational Semantics
This repository contains code for executable operational semantics. It provides
mechanisms to define the syntax and semantics of a programming language, and
your language can be executed based off the semantics you provide.

Currently, there is an implementation for a boolean language that uses `true`,
`false`, `if`, `and`, `or`, and `not` constructs.

The system is Turing complete, and so can encapsulate the semantics of any
programming language. I plan to provide example semantics of:
- Lambda calculus
- A simple arithmetic language
- A simple procedural language with loops

## Plugin System
The engine assumes that all syntax is represented as trees. This means integers
are represented as a chain of successors on some zero term. It also means that
the semantics for lambda calculus has about 30 inference rules and requires the
introduction of a dozen additional syntactic forms beyond abstraction and
application.

In order to support prototyping reasonably complex languages, I would like to
provide a plugin system where common semantic forms like arithmetic, boolean
algebra, variables, and function application can be included in any set of
semantics, without defining them for each language.

## Static Analysis of Semantics
I would like to provide static analysis on the semantics to determine properties
of them. Some of these are undecidable.
- The semantics are non-ambiguous: there is no term for which more than one
evaluation rule might apply
- Every value is a normal form. No element in the subset of the language defined
to be a value can be evaluated with any evaluation rule.
- Every normal form is a value. The subset of the language defined to be values
include all normal forms of the language, which cannot be evaluated further
    - I am fairly confident this is undecidable: the halting problem can be
    reduced to this
    - I could do dynamic runtime checks that enumerate programs in the language
    and verify that all programs evaluate to values
- Termination of evaluation: determine if the semantics will always halt
    - This is undecidable in the general case: it would directly solve the
    halting problem
- Equivalence of semantics
    - I am fairly confident this is undecidable because it would be equivalent
    to detecting if turing machines are equivalent, which is undecidable
