% menu.pl
:- module(menu, [
    display_main_menu/1,
    display_difficulty_menu/1,
    display_load_menu/0
 ]).
 
:- use_module('ui_core').
:- use_module('navigation').
:- use_module('../utils').          
:- use_module('../constants').      
:- use_module('../save_load').      
 
 display_main_menu(Choice) :-
     ui_core:clear_screen,
     ui_core:draw_title,
     format("~n[1] Novo Jogo~n", []),
     format("[2] Carregar Jogo~n", []),
     format("[3] Sair~n~n", []),
     write("Selecione uma opção: "),
     utils:get_valid_input(1, 3, Choice).
 
 display_difficulty_menu(Difficulty) :-
     ui_core:clear_screen,
     ui_core:draw_title,
     write("Escolha a dificuldade:"), nl,
     write("[1] easy"), nl,
     write("[2] medium"), nl,
     write("[3] hard"), nl,
     write("[0] Voltar"), nl,
     write("Opção: "),
     utils:get_valid_input(0, 3, Opt),
     ( Opt = 0 ->
          Difficulty = none
     ;  nth1(Opt, [easy, medium, hard], Difficulty)
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
 
