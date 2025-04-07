%%% Módulo game_state: Gerencia o estado do jogo de quebra-cabeça
%%% Principais funcionalidades:
%%% - Inicialização do tabuleiro
%%% - Marcação de células
%%% - Verificação de vitória
%%% - Validação de movimentos
:- module(game_state, [
    mark_cell/4,     % Marca uma célula no tabuleiro
    check_victory/2, % Verifica condição de vitória
    init_game/2,     % Inicializa novo jogo
    valid_coordinates/3, % Valida posições no tabuleiro
    game_state/5     % Estrutura de dados do estado do jogo
]).

:- dynamic game_state/5.  % Permite alteração dinâmica do estado do jogo

%%% INICIALIZAÇÃO DO JOGO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% init_game(+Puzzle, -GameState)
% Inicializa um novo estado de jogo com:
% - Grade vazia do mesmo tamanho da solução
% - 3 vidas
% - Estado não resolvido (false)
% - Posição inicial (0,0)
%
% Parâmetros:
%   Puzzle - Estrutura do puzzle contendo a solução (game/4)
%   GameState - Estado inicial do jogo (game_state/5)
init_game(Game, game_state(Grid, 3, Game, false, (0,0))) :-
    Game = game(Solution, _, _, _),    % Extrai a solução do puzzle
    maplist(empty_row, Solution, Grid). % Cria grade vazia baseada na solução

%% empty_row(+Row, -EmptyRow)
% Cria uma linha vazia com o mesmo comprimento da linha de referência
% Cada célula é preenchida com o átomo 'empty'
empty_row(Row, EmptyRow) :-
    length(Row, Len),          % Obtém comprimento da linha original
    length(EmptyRow, Len),     % Cria nova linha com mesmo comprimento
    maplist(=(empty), EmptyRow). % Preenche todas as células com 'empty'

%%% OPERAÇÕES NO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% mark_cell(+GameState, +MarkType, -NewGameState, -Msg)
% Marca a célula na posição atual (X,Y) com o tipo especificado
% Retorna novo estado do jogo e mensagem de confirmação
%
% Parâmetros:
%   GameState - Estado atual do jogo
%   MarkType - Tipo de marcação (filled, crossed, etc)
%   NewGameState - Estado atualizado após marcação
%   Msg - Mensagem de retorno ('Célula atualizada')
mark_cell(GameState, MarkType, NewGameState, Msg) :-
    GameState = game_state(Grid, Lives, Game, Solved, (X, Y)),
    update_grid(Grid, X, Y, MarkType, NewGrid), % Atualiza a grade
    NewGameState = game_state(NewGrid, Lives, Game, Solved, (X, Y)),
    Msg = 'Célula atualizada'.

%% update_grid(+Grid, +X, +Y, +Value, -NewGrid)
% Atualiza o valor de uma célula específica na posição (X,Y)
% X = índice da linha, Y = índice da coluna
update_grid(Grid, X, Y, Value, NewGrid) :-
    nth0(X, Grid, Row),               % Obtém a linha X
    replace_nth(Row, Y, Value, NewRow), % Substitui célula Y
    replace_nth(Grid, X, NewRow, NewGrid). % Atualiza linha na grade

%% replace_nth(+List, +Index, +Elem, -NewList)
% Substitui o elemento na posição Index da List por Elem
% Caso base: substituição no início da lista
replace_nth([_|T], 0, Elem, [Elem|T]).
% Caso recursivo: percorre lista até encontrar o índice
replace_nth([H|T], Index, Elem, [H|NewT]) :-
    Index > 0,
    Index1 is Index - 1,
    replace_nth(T, Index1, Elem, NewT).

%%% VERIFICAÇÃO DE VITÓRIA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% check_victory(+GameState, -Result)
% Verifica se o jogador completou o puzzle corretamente
% Compara a grade atual com a solução oficial
%
% Parâmetros:
%   GameState - Estado do jogo a verificar
%   Result - true se grade = solução, false caso contrário
check_victory(GameState, true) :-
    GameState = game_state(Grid, _, game(Solution, _, _, _), _, _),
    compare_grids(Grid, Solution). % Comparação exata das grades
check_victory(_, false).           % Caso contrário, retorna false

%% compare_grids(+Grid1, +Grid2)
% Compara duas grades linha por linha, célula por célula
compare_grids([], []). % Caso base: grades vazias
compare_grids([R1|Rest1], [R2|Rest2]) :-
    R1 == R2,           % Compara linhas
    compare_grids(Rest1, Rest2). % Recursão para próximas linhas

%%% VALIDAÇÃO DE COORDENADAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% valid_coordinates(+Grid, +X, +Y)
% Verifica se (X,Y) são coordenadas válidas dentro da grade
%
% Parâmetros:
%   Grid - Grade do jogo
%   X - Coordenada vertical (linha)
%   Y - Coordenada horizontal (coluna)
valid_coordinates(Grid, X, Y) :-
    length(Grid, NumRows),    % Obtém número de linhas
    NumRows > 0,             % Garante grade não vazia
    nth0(0, Grid, FirstRow),  % Obtém primeira linha
    length(FirstRow, NumCols), % Obtém número de colunas
    X >= 0, X < NumRows,     % Verifica X dentro dos limites
    Y >= 0, Y < NumCols.     % Verifica Y dentro dos limites