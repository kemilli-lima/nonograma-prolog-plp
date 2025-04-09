% Módulo logic.pl - Lógica Principal do Jogo Nonograma
%
% Responsável pelas regras essenciais do jogo:
% - Controle de vitória/derrota
% - Validação de jogadas
% - Sistema de dicas
% - Gerenciamento de vidas

:- module(logic, [
    check_victory/1,            % Verifica se o jogador venceu
    update_cell_with_check/5,   % Atualiza célula com validação
    give_hint/3,                % Fornece dica ao jogador  
    is_game_over/1              % Verifica se o jogo terminou
]).

:- use_module(game_state).  % Utiliza funções auxiliares do módulo game_state

% =============================================
% CONTROLE DE VITÓRIA
% =============================================

%% check_victory(+GameState)
% Verifica se o jogador completou o puzzle corretamente
% comparando o tabuleiro atual com a solução oficial
%
% Parâmetros:
%   GameState - Estado atual do jogo (game_state/5)
check_victory(GameState) :-
    game_state:check_victory(GameState, true). % Chama predicado auxiliar

% =============================================
% SISTEMA DE JOGADAS VALIDADAS
% =============================================

%% update_cell_with_check(+GameState, +(X,Y), +MarkType, -NewGameState, -Msg)
% Atualiza uma célula verificando se a jogada está correta:
% - Se correta: marca a célula
% - Se errada: decrementa uma vida
%
% Parâmetros:
%   GameState - Estado atual do jogo
%   (X,Y) - Posição da célula a marcar
%   MarkType - Tipo de marcação (filled/crossed)
%   NewGameState - Novo estado do jogo
%   Msg - Mensagem de feedback para o jogador
update_cell_with_check(GameState, (X,Y), MarkType, NewGameState, Msg) :-
    % Extrai componentes do estado do jogo
    GameState = game_state(Grid, Lives, game(Solution, RowHints, ColHints, Difficulty), Solved, _),
    
    % Obtém valor atual da célula
    nth0(X, Grid, Row),
    nth0(Y, Row, CurrentValue),
    
    % Verifica se a célula já está marcada
    (   CurrentValue \= empty
    ->  % Célula já marcada - não faz nada e retorna mensagem
        NewGameState = GameState,
        (   CurrentValue == filled
        ->  Msg = 'Essa célula já foi preenchida!'
        ;   Msg = 'Essa célula já foi marcada com X!'
        )
    ;   % Célula vazia - procede com verificação normal
        % Obtém valor correto da solução oficial
        nth0(X, Solution, SolRow),
        nth0(Y, SolRow, CorrectValue),
        
        % Verifica se a jogada está correta
        (   MarkType == CorrectValue
        ->  % Caso CORRETO: atualiza o grid
            update_grid(Grid, X, Y, MarkType, NewGrid),
            NewGameState = game_state(NewGrid, Lives, game(Solution, RowHints, ColHints, Difficulty), Solved, (X,Y)),
            Msg = 'Jogada correta!'
        ;   % Caso ERRADO: decrementa vidas
            NewLives is max(0, Lives - 1),
            NewGameState = game_state(Grid, NewLives, game(Solution, RowHints, ColHints, Difficulty), Solved, (X,Y)),
            Msg = 'Jogada errada! Perdeu uma vida.'
        )
    ).

%% update_grid(+Grid, +X, +Y, +Value, -NewGrid)
% Atualiza uma posição específica no grid do jogo
%
% Parâmetros:
%   Grid - Grade atual do jogo
%   X - Coordenada X (linha)
%   Y - Coordenada Y (coluna)
%   Value - Novo valor para a célula
%   NewGrid - Grade atualizada
update_grid(Grid, X, Y, Value, NewGrid) :-
    nth0(X, Grid, Row),               % Obtém a linha X
    replace_nth(Row, Y, Value, NewRow), % Atualiza célula na linha
    replace_nth(Grid, X, NewRow, NewGrid). % Atualiza linha na grade

%% replace_nth(+List, +Index, +Elem, -NewList)
% Substitui elemento em posição específica de uma lista
%
% Caso base: substituição no início
replace_nth([_|T], 0, Elem, [Elem|T]).
% Caso recursivo: procura posição
replace_nth([H|T], Index, Elem, [H|NewT]) :-
    Index > 0,
    Index1 is Index - 1,
    replace_nth(T, Index1, Elem, NewT).

% =============================================
% SISTEMA DE DICAS
% =============================================

%% give_hint(+GameState, -NewGameState, -Message)
% Fornece dica ao jogador corrigindo a primeira célula errada encontrada
%
% Parâmetros:
%   GameState - Estado atual do jogo
%   NewGameState - Estado após aplicação da dica
%   Message - Mensagem com posição corrigida
give_hint(GameState, NewGameState, Message) :-
    GameState = game_state(CurrentGrid, _Lives, Game, _Solved, _Pos),
    Game = game(Solution, _, _, _),
    
    % Procura primeira célula incorreta
    find_incorrect_cell(CurrentGrid, Solution, 0, 0, (FoundX, FoundY)),
    
    (   FoundX >= 0
    ->  % Corrige célula errada
        nth0(FoundX, Solution, SolRow),
        nth0(FoundY, SolRow, CorrectValue),
        update_cell_with_check(GameState, (FoundX, FoundY), CorrectValue, NewGameState, _),
        format(atom(Message), 'Dica aplicada na posição (~d,~d)', [FoundX,FoundY])
    ;   % Puzzle já está completo
        Message = 'Puzzle já completo!',
        NewGameState = GameState
    ).

%% find_incorrect_cell(+CurrentGrid, +Solution, +X, +Y, -Pos)
% Encontra coordenadas da primeira célula que difere da solução
%
% Parâmetros:
%   CurrentGrid - Grade atual do jogador
%   Solution - Grade solução
%   X, Y - Coordenadas atuais de busca
%   Pos - (X,Y) da primeira diferença encontrada ou (-1,-1)
find_incorrect_cell([], [], _, _, (-1,-1)). % Fim do tabuleiro
find_incorrect_cell([CRow|GridRest], [SRow|SolRest], X, _, Pos) :-
    find_incorrect_in_row(CRow, SRow, X, 0, PosInRow),
    (   PosInRow == (-1,-1)
    ->  % Não encontrou na linha, avança para próxima
        X1 is X + 1,
        find_incorrect_cell(GridRest, SolRest, X1, 0, Pos)
    ;   Pos = PosInRow  % Encontrou diferença
    ).

%% find_incorrect_in_row(+CurrentRow, +SolutionRow, +X, +Y, -Pos)
% Verifica células em uma linha específica
find_incorrect_in_row([], [], _, _, (-1,-1)). % Fim da linha
find_incorrect_in_row([C|CRest], [S|SRest], X, Y, Pos) :-
    (   C \= S
    ->  Pos = (X,Y)  % Encontrou diferença
    ;   % Continua busca na linha
        Y1 is Y + 1,
        find_incorrect_in_row(CRest, SRest, X, Y1, Pos)
    ).

% =============================================
% CONTROLE DE DERROTA
% =============================================

%% is_game_over(+GameState)
% Verifica condição de game over (vidas <= 0)
%
% Parâmetros:
%   GameState - Estado do jogo a verificar
is_game_over(game_state(_, Lives, _, _, _)) :-
    Lives =< 0.  % Jogo termina quando vidas acabam