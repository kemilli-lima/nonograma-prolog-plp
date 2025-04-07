:- module(menu, [
    display_main_menu/1,
    display_difficulty_menu/1,
    display_load_menu/0,
    ask_player_name/0,
    player_name/1
]).

:- dynamic player_name/1.

:- use_module('ui_core').
:- use_module('navigation').
:- use_module('../utils', [get_valid_input/3]).
:- use_module('../constants').      
:- use_module('../save_load').

ask_player_name :-
    ui_core:draw_title,
    constants:title_color(TitleColor),
    constants:reset_color(Reset),
    format("~sBem-vindo(a) ao jogo! Qual o seu nome?~s~n", [TitleColor, Reset]),
    read_line_to_string(user_input, Name),
    retractall(player_name(_)),
    assertz(player_name(Name)).

display_main_menu(Choice) :-
    ui_core:clear_screen,
    ui_core:draw_title,
    constants:title_color(TitleColor),
    constants:reset_color(Reset),

    nl,
    player_name(Name),

    nl,
    format("~sOlá ~w~s! Escolha uma opção:~n~n", [TitleColor, Name, TitleColor]),

    format("~s╔══════════════════════╗~n", [TitleColor]),
    format("~s║   1 ● Novo Jogo       ║~n", [TitleColor]),
    format("~s║   2 ● Carregar Jogo   ║~n", [TitleColor]),
    format("~s║   3 ● Sair            ║~n", [TitleColor]),
    format("~s╚══════════════════════╝~n", [TitleColor]),
    format("~s", [Reset]),

    format("~s ▶ Opção: ~s", [TitleColor, Reset]),
    utils:get_valid_input(1, 3, Choice).

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
    format("~s║   ~s1 ● Fácil~s           ║~n", [TitleColor, Green, TitleColor]),
    format("~s║   ~s2 ● Médio~s           ║~n", [TitleColor, Orange, TitleColor]),
    format("~s║   ~s3 ● Difícil~s         ║~n", [TitleColor, Red, TitleColor]),
    format("~s║   ~s0 ● Voltar~s          ║~n", [TitleColor, Magenta, TitleColor]),
    format("~s╚══════════════════════╝~n", [TitleColor]),
    format("~s", [Reset]),

    format("~s ▶ Opção: ~s", [TitleColor, Reset]),
    utils:get_valid_input(0, 3, Opt),
    ( Opt = 0 ->
        Difficulty = none
    ; nth1(Opt, [easy, medium, hard], Difficulty)
    ).
    
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

print_saved_games([]).
print_saved_games([S|Rest]) :-
    format("~s~n", [S]),
    print_saved_games(Rest).