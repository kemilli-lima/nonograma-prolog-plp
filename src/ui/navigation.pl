% Módulo navigation.pl - Sistema de Navegação e Controles
%
% Responsável por:
% - Gerenciar o loop principal do jogo
% - Processar entradas do usuário
% - Controlar movimento do cursor e ações
% - Gerenciar estados do jogo

:- module(navigation, [
    start_game_loop/1,  % Inicia o jogo
    handle_input/3      % Processa entrada do usuário
]).

:- use_module(ui_core).              % Para renderização da interface
:- use_module('../core/logic').      % Para regras do jogo
:- use_module('../utils').           % Para utilitários
:- use_module('../core/game_state'). % Para manipulação do estado
:- use_module('../save_load').       % Para operações de save/load

% =============================================
% LOOP PRINCIPAL DO JOGO
% =============================================

%% start_game_loop(+GameState)
% Inicia o loop principal do jogo com estado inicial
%
% Parâmetros:
%   GameState - Estado inicial do jogo (game_state/5)
%
% Fluxo:
% 1. Verifica condições de vitória/derrota
% 2. Renderiza interface
% 3. Captura entrada do usuário
% 4. Processa entrada e atualiza estado
% 5. Repete até término
start_game_loop(GameState) :-
    game_loop(GameState).


%% game_loop(+GameState)
% Implementação recursiva do loop principal
%
% Casos de término:
% - Vitória do jogador (check_victory/1)
% - Game Over (is_game_over/1)
game_loop(GameState) :-
    % Verifica condições de término
    ( logic:check_victory(GameState) -> 
         ui_core:show_victory(GameState),       % Vitória
         utils:pause(1),
         halt
    ; logic:is_game_over(GameState) -> 
         ui_core:show_game_over(GameState),     % Derrota
         utils:pause(1),
         halt
    ; % Caso contínuo:
      ui_core:draw_ui(GameState),    % Renderiza interface
      utils:get_single_char(Key),    % Captura tecla
      handle_input(GameState, Key, NewGameState), % Processa entrada
      game_loop(NewGameState)        % Recursão com novo estado
    ).

% =============================================
% MANIPULAÇÃO DE ENTRADAS
% =============================================

%% handle_input(+GameState, +Key, -NewGameState)
% Processa teclas pressionadas e retorna novo estado
%
% Teclas suportadas:
% - WASD: Movimento do cursor
% - F: Preencher célula
% - M: Marcar célula como vazia
% - H: Pedir dica
% - V: Salvar jogo
% - Q: Sair
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
    ; Key = 'v' ->
        read_line_to_string(user_input, _Discard),
        write("Digite o nome do arquivo de save (sem espaços): "),
        read_line_to_string(user_input, SaveStr),
        atom_string(SaveName, SaveStr),


        write("[DEBUG] Tentando salvar...\n"),

        (   catch(
                (save_load:save_game(SaveName, GameState),
                write("[DEBUG] save_game executado com sucesso.\n")
                ),
                Error,
                (   format("❌ Erro capturado: ~w~n", [Error]),
                    fail
                )
            )
        ->  write("Jogo salvo com sucesso!"), nl
        ;   write("O jogo *não* foi salvo.\n")
        ),

        utils:pause(2),
        NewGameState = GameState


    ; Key = 'q' -> halt % Sair
    ; NewGameState = GameState % Tecla inválida - mantém estado
    ).

% =============================================
% CONTROLE DO CURSOR
% =============================================

%% move_cursor(+GameState, +Direction, -NewGameState)
% Atualiza posição do cursor mantendo dentro dos limites
%
% Direções suportadas:
% - up, down, left, right
move_cursor(GameState, Direction, NewGameState) :-
    GameState = game_state(Grid, Lives, Game, Solved, (X,Y)),
    grid_dimensions(Grid, Rows, Cols), % Obtém limites do tabuleiro
    new_position(Direction, (X,Y), Rows, Cols, (NewX,NewY)), % Calcula nova pos
    % Cria novo estado com posição atualizada
    NewGameState = game_state(Grid, Lives, Game, Solved, (NewX, NewY)).


%% new_position(+Direction, +CurrentPos, +MaxRows, +MaxCols, -NewPos)
% Calcula nova posição garantindo que esteja dentro dos limites
new_position(up, (X,Y), _Rows, _Cols, (NX,Y)) :-
    NX is max(0, X-1). % Não passa do topo
new_position(down, (X,Y), Rows, _Cols, (NX,Y)) :-
    NX is min(Rows-1, X+1). % Não passa da base
new_position(left, (X,Y), _Rows, _Cols, (X,NY)) :-
    NY is max(0, Y-1). % Não passa da esquerda
new_position(right, (X,Y), _Rows, Cols, (X,NY)) :-
    NY is min(Cols-1, Y+1). % Não passa da direita

% =============================================
% UTILITÁRIOS
% =============================================

%% grid_dimensions(+Grid, -Rows, -Cols)
% Retorna dimensões do tabuleiro (linhas × colunas)
grid_dimensions(Grid, Rows, Cols) :-
    length(Grid, Rows), % Número de linhas
    ( Rows > 0 ->
         nth0(0, Grid, FirstRow),
         length(FirstRow, Cols) % Número de colunas
    ; Cols = 0 ). % Tabuleiro vazio