% Módulo controller.pl - Controlador Principal da Aplicação
%
% Responsável por:
% - Gerenciar o fluxo principal da aplicação
% - Coordenar a interação entre módulos
% - Tratar eventos globais e sinais do sistema
% - Inicializar e finalizar o sistema corretamente
:- module(controller, [start_application/0]).   % Define o módulo principal e exporta o ponto de entrada

% =============================================
% DEPENDÊNCIAS DO MÓDULO
% =============================================
:- use_module('ui/menu').            % Para interação com menus
:- use_module('ui/navigation').      % Para controle do jogo principal
:- use_module('ui/ui_core').         % Para interface gráfica
:- use_module('save_load').          % Para operações de save/load
:- use_module('core/game_state').    % Para manipulação do estado do jogo
:- use_module('core/puzzle_loader'). % Para carregamento de quebra-cabeças
:- use_module('constants').          % Para constantes do sistema
:- use_module('utils').              % Para utilitários auxiliares

% =============================================
% INICIALIZAÇÃO DA APLICAÇÃO
% =============================================

%% start_application
% Ponto de entrada principal do sistema
%
% Fluxo de execução:
% 1. Inicializa o sistema de UI
% 2. Garante que o diretório de saves existe
% 3. Registra handlers para sinais do sistema
% 4. Solicita o nome do jogador
% 5. Entra no loop principal
start_application :-                                    
    ui_core:init_ui,                    % Inicializa subsistema gráfico                                                   
    save_load:ensure_save_directory,    % Cria diretório de saves se necessário                    
    register_cleanup_handlers,          % Configura tratadores de sinais
    menu:ask_player_name,               % Obtém nome do jogador
    main_loop.                          % Inicia loop principal                                          

% =============================================
% LOOP PRINCIPAL E PROCESSAMENTO DE COMANDOS
% =============================================

%% main_loop
% Loop principal que gerencia o fluxo da aplicação
%
% Comportamento:
% - Exibe menu principal repetidamente
% - Processa escolhas do usuário
% - Mantém aplicação rodando até saída explícita
main_loop :-                                            
    menu:display_main_menu(Choice),                     % Mostra menu e captura escolha
    process_choice(Choice),                             % Executa ação correspondente
    main_loop.                                          % Loop infinito (até saída)

%% process_choice(+Choice)
% Processa a seleção do usuário no menu principal
%
% Casos:
% 1 - Novo jogo
% 2 - Carregar jogo
% 3 - Sair
% _ - Opção inválida
process_choice(1) :-                                    % Opção: Novo Jogo
    menu:display_difficulty_menu(Difficulty),           % Mostra submenu de dificuldade
    ( Difficulty = none ->                              % Usuário cancelou
         true                                           % Retorna silenciosamente
    ;  start_new_game(Difficulty)                       % Inicia novo jogo
    ).

process_choice(2) :-                                    % Opção: Carregar Jogo
    menu:display_load_menu.                             % Mostra menu de carregamento

process_choice(3) :-                                    % Opção: Sair
    ui_core:clear_screen,
    write('Até logo!'), nl,
    halt.                                               % Encerra aplicação

process_choice(_) :-                                    % Opção inválida
    write('Opção inválida.'), nl.                       % Feedback ao usuário

% =============================================
% GERENCIAMENTO DE JOGO
% =============================================

%% start_new_game(+Difficulty)
% Inicializa um novo jogo com a dificuldade especificada
%
% Parâmetros:
%   Difficulty - Nível de dificuldade (easy/medium/hard)
%
% Fluxo:
% 1. Carrega puzzle adequado
% 2. Inicializa estado do jogo
% 3. Inicia loop de jogo
start_new_game(Difficulty) :-                           
    puzzle_loader:load_puzzle(Difficulty, Game),        % Obtém puzzle aleatório
    game_state:init_game(Game, State),                  % Cria estado inicial
    navigation:start_game_loop(State).                  % Transfere controle para o jogo

% =============================================
% TRATAMENTO DE SINAIS E FINALIZAÇÃO
% =============================================

%% register_cleanup_handlers
% Registra tratadores para sinais de interrupção
%
% Objetivo:
% - Garantir saída segura ao receber Ctrl+C ou SIGTERM
register_cleanup_handlers :-                            
    on_signal(int, _, cleanup_and_exit),                % Captura Ctrl+C (SIGINT)
    on_signal(term, _, cleanup_and_exit).               % Captura SIGTERM

%% cleanup_and_exit(+Signal)
% Rotina de limpeza antes da saída forçada
%
% Parâmetros:
%   Signal - Sinal recebido (não utilizado)
cleanup_and_exit(_) :-                                  % Limpeza dos sistemas de UI
    ui_core:cleanup_systems,                            % Termina execução imediatamente
    halt.
