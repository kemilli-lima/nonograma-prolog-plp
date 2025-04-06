% Módulo logic.pl - Lógica Principal do Jogo
%
% Responsável pelas regras e mecânicas essenciais do Nonograma:
% - Verificação de vitória/derrota
% - Atualização do tabuleiro com validação
% - Sistema de dicas
% - Controle de vidas

:- module(logic, [
    check_victory/1,            % Verifica condição de vitória
    update_cell_with_check/5,   % Atualiza célula com validação
    give_hint/3,                % Fornece dica ao jogador  
    is_game_over/1              % Verifica condição de derrota
]).

:- use_module(game_state).  % Para acesso ao estado do jogo

% ======================
% CONTROLE DE VITÓRIA
% ======================

%% check_victory(+GameState)
% Verifica se o jogador completou o puzzle corretamente
% comparando o tabuleiro atual com a solução oficial
check_victory(GameState) :-
    game_state:check_victory(GameState, true).

% ======================
% SISTEMA DE JOGADAS
% ======================

%% update_cell_with_check(+GameState, +(X,Y), +MarkType, -NewGameState, -Msg)
% Atualiza uma célula com verificação de acerto:
% 1. Obtém o valor correto da solução oficial
% 2. Compara com a jogada do usuário
% 3. Atualiza estado do jogo conforme resultado
update_cell_with_check(GameState, (X,Y), MarkType, NewGameState, Msg) :-
    % Extrai componentes do estado
    GameState = game_state(Grid, Lives, game(Solution, RowHints, ColHints, Difficulty), Solved, _),
    
    % Obtém valor correto da solução
    nth0(X, Solution, SolRow),
    nth0(Y, SolRow, CorrectValue),
    
    % Verifica se a jogada está correta
    (   MarkType == CorrectValue
    ->  % Jogada correta: atualiza o grid
        update_grid(Grid, X, Y, MarkType, NewGrid),
        NewGameState = game_state(NewGrid, Lives, game(Solution, RowHints, ColHints, Difficulty), Solved, (X,Y)),
        Msg = 'Jogada correta!'
    ;   % Jogada errada: decrementa vidas
        NewLives is max(0, Lives - 1),
        NewGameState = game_state(Grid, NewLives, game(Solution, RowHints, ColHints, Difficulty), Solved, (X,Y)),
        Msg = 'Jogada errada! Perdeu uma vida.'
    ).

% Atualiza uma posição específica no grid
update_grid(Grid, X, Y, Value, NewGrid) :-
    nth0(X, Grid, Row),
    replace_nth(Row, Y, Value, NewRow),
    replace_nth(Grid, X, NewRow, NewGrid).

% Substitui elemento em uma posição da lista
replace_nth([_|T], 0, Elem, [Elem|T]).
replace_nth([H|T], Index, Elem, [H|NewT]) :-
    Index > 0,
    Index1 is Index - 1,
    replace_nth(T, Index1, Elem, NewT).

% ======================
% SISTEMA DE DICAS
% ======================

%% give_hint(+GameState, -NewGameState, -Message)
% Fornece dica ao encontrar a primeira célula incorreta:
% 1. Varre o tabuleiro procurando discrepâncias com a solução
% 2. Corrige a primeira célula errada encontrada
% 3. Retorna mensagem com posição corrigida
give_hint(GameState, NewGameState, Message) :-
    GameState = game_state(CurrentGrid, _Lives, Game, _Solved, _Pos),
    Game = game(Solution, _, _, _),
    
    % Encontra a primeira célula incorreta
    find_incorrect_cell(CurrentGrid, Solution, 0, 0, (FoundX, FoundY)),
    
    (   FoundX >= 0
    ->  % Corrige a célula errada
        nth0(FoundX, Solution, SolRow),
        nth0(FoundY, SolRow, CorrectValue),
        update_cell_with_check(GameState, (FoundX, FoundY), CorrectValue, NewGameState, _),
        format(atom(Message), 'Dica aplicada na posição (~d,~d)', [FoundX,FoundY])
    ;   % Puzzle já completo
        Message = 'Puzzle já completo!',
        NewGameState = GameState
    ).

% Encontra a primeira célula que difere da solução
find_incorrect_cell([], [], _, _, (-1,-1)). % Caso base: fim do tabuleiro
find_incorrect_cell([CRow|GridRest], [SRow|SolRest], X, _, Pos) :-
    find_incorrect_in_row(CRow, SRow, X, 0, PosInRow),
    (   PosInRow == (-1,-1)
    ->  % Não encontrou nesta linha, avança para próxima
        X1 is X + 1,
        find_incorrect_cell(GridRest, SolRest, X1, 0, Pos)
    ;   Pos = PosInRow  % Encontrou na linha atual
    ).


% Verifica células em uma linha específica
find_incorrect_in_row([], [], _, _, (-1,-1)). % Fim da linha
find_incorrect_in_row([C|CRest], [S|SRest], X, Y, Pos) :-
    (   C \= S
    ->  % Encontrou discrepância
        Pos = (X,Y)
    ;   % Avança para próxima célula
        Y1 is Y + 1,
        find_incorrect_in_row(CRest, SRest, X, Y1, Pos)
    ).


% ======================
% CONTROLE DE DERROTA
% ======================

%% is_game_over(+GameState)
% Verifica condição de game over (vidas <= 0)
is_game_over(game_state(_, Lives, _, _, _)) :-
    Lives =< 0.