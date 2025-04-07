% controller.pl
:- module(controller, [start_application/0]).           % Define o módulo principal e exporta o ponto de entrada

:- use_module('ui/menu').
:- use_module('ui/navigation').
:- use_module('ui/ui_core').
:- use_module('save_load').
:- use_module('core/game_state').
:- use_module('core/puzzle_loader').
:- use_module('constants').
:- use_module('utils').


start_application :-                                    
    ui_core:init_ui,                                   
    save_load:ensure_save_directory,                    
    register_cleanup_handlers,
    menu:ask_player_name,  % Garante que o nome será armazenado
    main_loop.                                                                    

main_loop :-                                            
    menu:display_main_menu(Choice),                     % Exibe o menu principal e obtém a escolha do usuário
    process_choice(Choice),                             
    main_loop.                                          % Repete o loop (até o usuário sair)

process_choice(1) :-                                    
    menu:display_difficulty_menu(Difficulty),           % Mostra menu de dificuldade
    ( Difficulty = none ->                              % Se o usuário cancelar
         true
    ;  start_new_game(Difficulty)                       % Caso contrário, inicia um novo jogo
    ).
process_choice(2) :-                                    
    menu:display_load_menu.                             % Chama menu de carregamento de jogo salvo

process_choice(3) :-                                    
    ui_core:clear_screen,
    write('Até logo!'), nl,
    halt.                                               % Encerra o programa

process_choice(_) :-                                    
    write('Opção inválida.'), nl.                       % Trata entradas inválidas

start_new_game(Difficulty) :-                           
    puzzle_loader:load_puzzle(Difficulty, Game),        % Carrega o puzzle com base na dificuldade
    game_state:init_game(Game, State),                  % Inicializa o estado do jogo
    navigation:start_game_loop(State).                  % Começa o loop de interação com o jogo

register_cleanup_handlers :-                            
    on_signal(int, _, cleanup_and_exit),                % Lida com Ctrl+C ou sinais de interrupção
    on_signal(term, _, cleanup_and_exit).

cleanup_and_exit(_) :-                                  
    ui_core:cleanup_systems,                            % Libera recursos da interface
    halt.
