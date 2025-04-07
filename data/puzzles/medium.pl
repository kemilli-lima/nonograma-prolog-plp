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
        [[1], [3], [5], [3], [1]],  % rowsHints
        [[1], [3], [5], [3], [1]],  % colsHints
        medium
    ),

    puzzle(
       [  [marked, marked, filled, marked, marked],
          [marked, filled, filled, filled, marked],
          [filled, filled, filled, filled, filled],
          [marked, filled, filled, filled, marked],
          [marked, marked, filled, marked, marked] ],
        [[1], [3], [5], [3], [1]],      % rowsHints
        [[1], [3], [5], [3], [1]],      % colsHints
        medium 
    ),

    puzzle(
      [ [filled, filled, marked, filled, filled],
        [filled, filled, marked, filled, filled],
        [marked, marked, filled, marked, marked],
        [marked, filled, marked, filled, marked],
        [filled, marked, marked, marked, filled] ],
      [[2, 2], [2, 2], [1], [1, 1], [1, 1]],    % rowsHints
      [[2, 1], [2, 1], [1], [2, 1], [2, 1]],    % colsHints 
      medium
    ),

    puzzle(
      [ [marked, marked, filled, marked, marked],
        [marked, filled, filled, filled, marked],
        [filled, filled, filled, filled, filled],
        [marked, filled, marked, filled, marked],
        [marked, filled, marked, filled, marked] ],
      [[1], [3], [5], [1, 1], [1, 1]],     % rowsHints
      [[1], [4], [3], [4], [1]],           % colsHints
      medium
    )
]).