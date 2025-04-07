% Módulo menu.pl - Sistema de Menus do Nonograma
%
% Responsável por toda a interface de menus do jogo:
% - Menu principal
% - Seleção de dificuldade
% - Carregamento de jogos salvos
% - Captura do nome do jogador
% - Exibição de opções coloridas

:- module(menu, [
    display_main_menu/1,        % Exibe menu principal e captura escolha
    display_difficulty_menu/1, % Exibe menu de dificuldades
    display_load_menu/0,       % Exibe menu de carregamento
    ask_player_name/0,         % Solicita nome do jogador
    player_name/1              % Fatos dinâmicos para armazenar nome
]).

:- dynamic player_name/1.      % Permite armazenar/consultar nome do jogador

% Módulos necessários
:- use_module('ui_core').      % Para funções básicas de UI
:- use_module('navigation').   % Para controle de fluxo
:- use_module('../utils', [get_valid_input/3]). % Para entrada validada
:- use_module('../constants'). % Para cores e constantes
:- use_module('../save_load'). % Para operações com saves

% =============================================
% CAPTURA DO NOME DO JOGADOR
% =============================================

%% ask_player_name
% Solicita e armazena o nome do jogador
%
% Fluxo:
% 1. Exibe título do jogo
% 2. Solicita entrada do usuário
% 3. Armazena dinamicamente o nome
ask_player_name :-
    ui_core:draw_title,
    constants:title_color(TitleColor),
    constants:reset_color(Reset),
    format("~sBem-vindo(a) ao jogo! Qual o seu nome?~s~n", [TitleColor, Reset]),
    read_line_to_string(user_input, Name),
    retractall(player_name(_)),
    assertz(player_name(Name)).

% =============================================
% MENU PRINCIPAL
% =============================================

%% display_main_menu(-Choice)
% Exibe o menu principal e captura a escolha do jogador
%
% Parâmetros:
%   Choice - Opção selecionada (1-3)
display_main_menu(Choice) :-
    ui_core:clear_screen,
    ui_core:draw_title,
    constants:title_color(TitleColor),
    constants:reset_color(Reset),

    nl,
    player_name(Name),          % Obtém nome armazenado

    nl,
    format("~sOlá ~w~s! Escolha uma opção:~n~n", [TitleColor, Name, TitleColor]),

    format("~s╔══════════════════════╗~n", [TitleColor]),
    format("~s║   1 ● Novo Jogo      ║~n", [TitleColor]),
    format("~s║   2 ● Carregar Jogo  ║~n", [TitleColor]),
    format("~s║   3 ● Sair           ║~n", [TitleColor]),
    format("~s╚══════════════════════╝~n", [TitleColor]),
    format("~s", [Reset]),

    format("~s ▶ Opção: ~s", [TitleColor, Reset]),
    utils:get_valid_input(1, 3, Choice).

% =============================================
% MENU DE DIFICULDADE
% =============================================

%% display_difficulty_menu(-Difficulty)
% Exibe menu de seleção de dificuldade
%
% Parâmetros:
%   Difficulty - Dificuldade selecionada (easy/medium/hard/none)
display_difficulty_menu(Difficulty) :-
    ui_core:clear_screen,
    ui_core:draw_title,

    constants:title_color(TitleColor),
    constants:success_color(Green),
    constants:warning_color(Orange),
    constants:error_color(Red),
    constants:magenta_color(Magenta),
    constants:reset_color(Reset),

    nl,
    format("~sSelecione a dificuldade:~s~n~n", [TitleColor, TitleColor]),

    format("~s╔══════════════════════╗~n", [TitleColor]),
    format("~s║   ~s1 ● Fácil~s          ║~n", [TitleColor, Green, TitleColor]),
    format("~s║   ~s2 ● Médio~s          ║~n", [TitleColor, Orange, TitleColor]),
    format("~s║   ~s3 ● Difícil~s        ║~n", [TitleColor, Red, TitleColor]),
    format("~s║   ~s0 ● Voltar~s         ║~n", [TitleColor, Magenta, TitleColor]),
    format("~s╚══════════════════════╝~n", [TitleColor]),
    format("~s", [Reset]),

    format("~s ▶ Opção: ~s", [TitleColor, Reset]),
    utils:get_valid_input(0, 3, Opt),
    ( Opt = 0 ->
        Difficulty = none
    ; nth1(Opt, [easy, medium, hard], Difficulty)
    ).

% =============================================
% MENU DE CARREGAMENTO
% =============================================

%% display_load_menu
% Exibe lista de jogos salvos e permite carregar
%
% Fluxo:
% 1. Lista todos os saves disponíveis
% 2. Se vazio, mostra mensagem
% 3. Se não vazio, permite selecionar ou voltar    
display_load_menu :-
    ui_core:clear_screen,
    ui_core:draw_title,
    ( save_load:list_saved_games(Saves),
      Saves \= [] ->
          print_saved_games(Saves),
          write("Selecione um save (ou 0 para voltar): "),
          utils:get_valid_input(0, 100, Opt),
          ( Opt = 0 ->
                true
          ;  nth1(Opt, Saves, SaveName),
             save_load:load_game(SaveName, GameState),
             navigation:start_game_loop(GameState)
          )
    ;  write("Nenhum jogo salvo encontrado."), nl,
       utils:pause(2)
    ).

% =============================================
% AUXILIARES DE LISTAGEM
% =============================================

%% print_saved_games(+Saves)
% Imprime lista numerada de jogos salvos
%
% Parâmetros:
%   Saves - Lista de nomes de saves
print_saved_games(Saves) :-
    print_saved_games(Saves, 1).

%% print_saved_games(+Saves, +Index)
% Implementação recursiva com numeração
print_saved_games([], _).
print_saved_games([S|Rest], Index) :-
    format("~w. ~s~n", [Index, S]),
    NextIndex is Index + 1,
    print_saved_games(Rest, NextIndex).
