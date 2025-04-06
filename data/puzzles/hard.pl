% data/puzzles/hard.pl
:- module(hard, [puzzles/1]).

puzzles([
    % Puzzle 7x7
    puzzle(
        [ [marked, marked, marked, marked, marked, marked, marked],
          [marked, filled, filled, marked, filled, filled, marked],
          [filled, filled, filled, filled, filled, filled, filled],
          [filled, filled, filled, filled, filled, filled, filled],
          [marked, filled, filled, filled, filled, filled, marked],
          [marked, marked, filled, filled, filled, marked, marked],
          [marked, marked, marked, filled, marked, marked, marked] ],
        [[0], [2,2], [7], [7], [5], [3], [1]],
        [[2], [4], [5], [5], [5], [4], [2]],
        hard
    ),

    % Puzzle 10x10
    puzzle(
        [ [marked, marked, marked, marked, marked, filled, marked, marked, marked, marked],
          [marked, marked, marked, filled, filled, filled, filled, filled, marked, marked],
          [marked, marked, filled, filled, filled, filled, filled, filled, filled, marked],
          [marked, filled, filled, filled, filled, filled, filled, filled, filled, filled],
          [filled, filled, filled, filled, filled, filled, filled, filled, filled, filled],
          [filled, filled, filled, filled, filled, filled, filled, filled, filled, filled],
          [filled, marked, filled, marked, filled, filled, marked, filled, marked, filled],
          [marked, marked, marked, marked, marked, filled, marked, marked, marked, marked],
          [marked, marked, marked, filled, marked, filled, marked, marked, marked, marked],
          [marked, marked, marked, filled, filled, filled, marked, marked, marked, marked] ],
        [[1], [5], [7], [9], [10], [10], [1,1,2,1,1], [1], [1,1], [3]],
        [[3], [3], [5], [5,2], [6,1], [10], [5], [6], [4], [4]],
        hard
    ),

    % Puzzle 15x15
    puzzle(
        [ [marked,marked,marked,marked,marked,marked,marked,marked,marked,marked,marked,marked,filled,filled,marked],
          [marked,marked,marked,marked,marked,filled,filled,filled,filled,filled,marked,filled,marked,filled,filled],
          [marked,marked,marked,marked,filled,filled,filled,marked,marked,filled,filled,marked,marked,filled,filled],
          [marked,marked,filled,filled,filled,filled,filled,filled,filled,marked,filled,filled,filled,filled,marked],
          [marked,marked,filled,filled,filled,filled,filled,filled,filled,filled,filled,filled,filled,filled,marked],
          [marked,filled,filled,filled,marked,filled,filled,filled,filled,filled,filled,filled,filled,filled,marked],
          [marked,filled,filled,marked,filled,filled,filled,filled,filled,filled,filled,filled,marked,filled,marked],
          [marked,filled,filled,marked,filled,filled,filled,filled,filled,filled,filled,marked,filled,filled,marked],
          [marked,filled,filled,marked,filled,filled,filled,filled,filled,filled,marked,filled,marked,filled,marked],
          [marked,filled,filled,filled,filled,filled,filled,filled,filled,marked,filled,filled,marked,filled,marked],
          [marked,marked,filled,filled,filled,filled,filled,filled,marked,filled,filled,marked,filled,marked,marked],
          [marked,filled,marked,filled,filled,filled,filled,marked,filled,filled,filled,filled,filled,marked,marked],
          [filled,marked,marked,filled,filled,filled,marked,filled,marked,marked,filled,filled,marked,marked,marked],
          [filled,filled,filled,filled,filled,marked,filled,filled,filled,filled,marked,marked,marked,marked,marked],
          [marked,filled,filled,marked,marked,marked,marked,marked,marked,marked,marked,marked,marked,marked,marked] ],
          [[2],[5,1,2],[3,2,2],[7,4],[12],[3,9],[2,8,1],[2,7,2],[2,6,1,1],[8,2,1],[6,2,1],[1,4,5],[1,3,1,2],[5,4],[2]],
          [[2],[5,1,2],[8,2],[3,5],[3,8],[12],[11,1],[1, 8, 2],[1,7,1,1],[2,5,2,1],[6,4],[1,4,2,2],[1,3,1,2],[10],[2]],
          hard)
]).