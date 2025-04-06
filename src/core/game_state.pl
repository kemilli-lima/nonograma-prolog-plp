:- module(game_state, [
    mark_cell/4,
    check_victory/2,
    init_game/2,
    valid_coordinates/3,
    game_state/5  % Exporta o functor para manipulação direta, se necessário
]).

:- dynamic game_state/5.  % Permite modificar fatos game_state/5 dinamicamente (se usado em outro lugar)

%% init_game(+Puzzle, -GameState)
% Inicializa o estado do jogo com uma grade vazia, 3 vidas e posição inicial (0,0)
init_game(Game, game_state(Grid, 3, Game, false, (0,0))) :-
    Game = game(Solution, _, _, _),                    % Extrai a solução do puzzle
    maplist(empty_row, Solution, Grid).                % Cria uma grade vazia com mesmo tamanho da solução

empty_row(Row, EmptyRow) :-
    length(Row, Len),
    length(EmptyRow, Len),
    maplist(=(empty), EmptyRow).                       % Preenche a linha com 'empty'

%% mark_cell(+GameState, +MarkType, -NewGameState, -Msg)
% Marca a célula atual (posição X,Y) com o tipo especificado
mark_cell(GameState, MarkType, NewGameState, Msg) :-
    GameState = game_state(Grid, Lives, Game, Solved, (X, Y)),
    update_grid(Grid, X, Y, MarkType, NewGrid),        % Atualiza o conteúdo da célula
    NewGameState = game_state(NewGrid, Lives, Game, Solved, (X, Y)),
    Msg = 'Célula atualizada'.                         % Retorna mensagem de sucesso

update_grid(Grid, X, Y, Value, NewGrid) :-             % Substitui valor na célula (X,Y) da grade
    nth0(X, Grid, Row),
    replace_nth(Row, Y, Value, NewRow),
    replace_nth(Grid, X, NewRow, NewGrid).

replace_nth([_|T], 0, Elem, [Elem|T]).                 % Substitui o primeiro elemento
replace_nth([H|T], Index, Elem, [H|NewT]) :-           % Substitui o elemento no índice dado
    Index > 0,
    Index1 is Index - 1,
    replace_nth(T, Index1, Elem, NewT).

%% check_victory(+GameState, -Result)
% Verifica se o jogador venceu comparando grade atual com a solução
check_victory(GameState, true) :-
    GameState = game_state(Grid, _, game(Solution, _, _, _), _, _),
    compare_grids(Grid, Solution).                     % Vence se a grade for igual à solução
check_victory(_, false).

compare_grids([], []).
compare_grids([R1|Rest1], [R2|Rest2]) :-               % Compara cada linha da grade
    R1 == R2,
    compare_grids(Rest1, Rest2).

%% valid_coordinates(+Grid, +X, +Y)
% Verifica se a posição (X,Y) está dentro dos limites da grade
valid_coordinates(Grid, X, Y) :-
    length(Grid, NumRows),
    NumRows > 0,
    nth0(0, Grid, FirstRow),
    length(FirstRow, NumCols),
    X >= 0, X < NumRows,
    Y >= 0, Y < NumCols.

