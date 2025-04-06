% Módulo navigation.pl - Sistema de Navegação e Controles
% 
% Responsável por:
% - Gerenciar o loop principal do jogo
% - Processar entradas do usuário
% - Controlar movimento do cursor e ações

:- module(navigation, [
    start_game_loop/1,  % Inicia o jogo
    handle_input/3      % Processa entrada do usuário
]).

:- use_module(ui_core).              
:- use_module('../core/logic').      
:- use_module('../utils').           
:- use_module('../core/game_state'). 

% ======================
% LOOP PRINCIPAL DO JOGO
% ======================

% Inicia o loop do jogo com estado inicial
start_game_loop(GameState) :-
    game_loop(GameState).

% Loop principal que mantém o jogo rodando
game_loop(GameState) :-
    % Verifica condições de término
    ( logic:check_victory(GameState) -> 
         ui_core:show_victory,       % Vitória
         utils:pause(2),
         halt
    ; logic:is_game_over(GameState) -> 
         ui_core:show_game_over,     % Derrota
         utils:pause(2),
         halt
    ; % Caso contínuo:
      ui_core:draw_ui(GameState),    % Renderiza interface
      utils:get_single_char(Key),    % Captura tecla
      handle_input(GameState, Key, NewGameState), % Processa entrada
      game_loop(NewGameState)        % Recursão com novo estado
    ).

% ========================
% MANIPULAÇÃO DE ENTRADAS
% ========================

% Processa teclas pressionadas pelo usuário
handle_input(GameState, Key, NewGameState) :-
    GameState = game_state(_,_,_,_,(X,Y)), % Extrai posição atual
    ( Key = 'w' -> move_cursor(GameState, up, NewGameState)    % Cima
    ; Key = 's' -> move_cursor(GameState, down, NewGameState)  % Baixo
    ; Key = 'a' -> move_cursor(GameState, left, NewGameState)  % Esquerda
    ; Key = 'd' -> move_cursor(GameState, right, NewGameState) % Direita
    ; Key = 'f' -> % Preencher célula
        logic:update_cell_with_check(GameState, (X,Y), filled, NewGameState, Msg),
        write(Msg), nl, utils:pause(1)
    ; Key = 'm' -> % Marcar célula
        logic:update_cell_with_check(GameState, (X,Y), marked, NewGameState, Msg),
        write(Msg), nl, utils:pause(1)
    ; Key = 'h' -> % Pedir dica
        logic:give_hint(GameState, NewGameState, Msg),
        write(Msg), nl, utils:pause(1)
    ; Key = 'q' -> halt % Sair
    ; NewGameState = GameState % Tecla inválida - mantém estado
    ).

% ======================
% CONTROLE DO CURSOR
% ======================

% Move o cursor na direção especificada
move_cursor(GameState, Direction, NewGameState) :-
    GameState = game_state(Grid, Lives, Game, Solved, (X,Y)),
    grid_dimensions(Grid, Rows, Cols), % Obtém limites do tabuleiro
    new_position(Direction, (X,Y), Rows, Cols, (NewX,NewY)), % Calcula nova pos
    % Cria novo estado com posição atualizada
    NewGameState = game_state(Grid, Lives, Game, Solved, (NewX, NewY)).

% Calcula nova posição dentro dos limites do tabuleiro
new_position(up, (X,Y), _Rows, _Cols, (NX,Y)) :-
    NX is max(0, X-1). % Não passa do topo
new_position(down, (X,Y), Rows, _Cols, (NX,Y)) :-
    NX is min(Rows-1, X+1). % Não passa da base
new_position(left, (X,Y), _Rows, _Cols, (X,NY)) :-
    NY is max(0, Y-1). % Não passa da esquerda
new_position(right, (X,Y), _Rows, Cols, (X,NY)) :-
    NY is min(Cols-1, Y+1). % Não passa da direita

% ======================
% UTILITÁRIOS
% ======================

% Obtém dimensões do tabuleiro (linhas x colunas)
grid_dimensions(Grid, Rows, Cols) :-
    length(Grid, Rows), % Número de linhas
    ( Rows > 0 ->
         nth0(0, Grid, FirstRow),
         length(FirstRow, Cols) % Número de colunas
    ; Cols = 0 ). % Tabuleiro vazio