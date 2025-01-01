# Advent of Code 2024

## Ex-ante
Solutions to Advent of Code 2024 puzzles in SWI Prolog. My aim this
year is to try and make use of Prolog's unique features more and
avoid writing functional code which is tempting when working with
immutable data structures.

## Ex-post
* I tried in earnest to use Prolog's distinguishing features as
  much as possible, some times with great success (e.g. Q17).

* The code is mostly short, but also dense and laden with 2 letter
  variables.

* Here are the questions in which I used CLP(FD/B) to significant
  advantage: 7.1, 8, 13, 17, 24.1.

* Here are the questions in which I used SLG resolution (aka
  tabling) to significant advantage: 10, 11, 19.

* The code I've committed for 21.1 is wrong: I made the mistake
  comitting a refactor.

* My plan for 24 was to use CLP(B) to model both the given circuit
  and a working binary adder, and then for each incorrect output,
  test the dependent endpoints exhaustively until a fix is found.
  However, implementing this with CLP(B) turned out to be
  difficult and I eventually gave up on it in favour of a Python
  Z3 version.

I did AoC 2023 in Prolog too, and I feel this code is a significant
improvement over previous attempts. I think that Prolog makes these
puzzles harder to solve overall. Prolog is a kind of thought
scaffolding which requires a significant amount of sympathy for the
interpreter. For example, in question 24, working out what CLP(B)
was doing during backtracking and what constraints were active at
different parts of the code got quite exhausting. Other than what
felt like unrewarded cognitive load, speed of the code was a
constant issue, and the unavoidable verbosity that comes with
having to chain predicates together using intermediary variables,
when processing lists.

I think perhaps the biggest benefit of using Prolog for these
puzzles is as a an aide to tacitly understanding a different way
of expressing computing: Prolog clauses are logical but always
have a procedural interpretation which is "executed" through
SLD/SLG resolution.
