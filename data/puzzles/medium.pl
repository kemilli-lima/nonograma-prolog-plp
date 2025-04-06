% data/puzzles/medium.pl
:- module(medium, [puzzles/1]).

puzzles([
    % Puzzle 5x5
    puzzle(
        [ [marked, marked, filled, marked, marked],
          [marked, filled, filled, filled, marked],
          [filled, filled, filled, filled, filled],
          [marked, filled, filled, filled, marked],
          [marked, marked, filled, marked, marked] ],
        [[1], [3], [5], [3], [1]],
        [[1], [3], [5], [3], [1]],
        medium
    )
]).