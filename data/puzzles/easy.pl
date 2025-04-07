:- module(easy, [puzzles/1]).

puzzles([
    % Puzzle 4x4..
    puzzle(
        [ [filled, filled, filled, marked],
          [marked, filled, filled, filled],
          [filled, marked, filled, marked],
          [filled, filled, marked, filled] ],
        [[3], [3], [1,1], [2,1]],       % rowsHints
        [[1,2], [2,1], [3], [1,1]],     % colsHints
        easy
    ),
    
    % Puzzle 4x4 alternativo
    puzzle(
        [ [filled, marked, marked, filled],
          [filled, marked, marked, filled],
          [filled, filled, filled, filled],
          [filled, marked, marked, filled] ],
        [[1,1], [1,1], [4], [1,1]],     % rowsHints
        [[4], [1], [1], [4]],           % colsHints
        easy
    ),

    puzzle(
        [ [filled, filled, filled, marked],
          [marked, filled, filled, filled],
          [filled, marked, filled, marked],
          [filled, filled, marked, filled] ],
        [[3], [3], [1, 1], [2, 1] ],    % rowsHints
        [[1, 2], [2, 1], [3], [1, 1]],  % colsHints
        easy
    ),

    puzzle(
        [ [filled, marked, marked, filled],
          [filled, marked, marked, filled],
          [filled, filled, filled, filled],
          [filled, marked, marked, filled] ],
          [ [1, 1], [1, 1], [4], [1, 1]], % rowsHints
          [ [4], [1], [1], [4]],          % colsHints
          easy
    ),

    puzzle(
        [ [filled, marked, marked, filled],
          [filled, filled, filled, filled],
          [filled, filled, filled, filled],
          [marked, filled, filled, marked] ],

          [[1, 1], [4], [4], [2] ],       % rowsHints
          [[3], [3], [3], [3] ],          % colsHints
          easy
    )

]).