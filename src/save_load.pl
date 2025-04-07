% Módulo save_load.pl - Sistema de Salvamento e Carregamento do Nonograma
%
% Responsável por todas as operações relacionadas a persistência do jogo:
% - Salvamento do estado do jogo
% - Carregamento de jogos salvos
% - Listagem de saves disponíveis
% - Exclusão de saves
% - Validação de nomes de arquivos

:- module(save_load, [
    save_game/2,            % Salva estado do jogo em arquivo
    load_game/2,            % Carrega jogo salvo
    list_saved_games/1,     % Lista todos os jogos salvos
    delete_saved_game/1,    % Remove jogo salvo
    valid_save_name/1,      % Valida nome para salvamento
    ensure_save_directory/0 % Garante existência do diretório de saves
]).

% Dependências
:- use_module(library(filesex)).    % Para operações com arquivos
:- use_module(library(lists)).      % Para manipulação de listas
:- use_module('core/game_state').   % Para manipulação do estado do jogo

:- dynamic game_state:valid_coordinates/3.

% =============================================
% CONFIGURAÇÃO DE DIRETÓRIO
% =============================================

%% save_directory(-Dir)
% Define o diretório padrão para salvamento
save_directory('data/saves').

% =============================================
% OPERAÇÕES PRINCIPAIS
% =============================================

%% save_game(+FileName, +GameState)
% Salva o estado do jogo em um arquivo
%
% Parâmetros:
%   FileName - Nome do arquivo de salvamento
%   GameState - Estado do jogo a ser salvo
%
% Fluxo:
% 1. Valida nome do arquivo
% 2. Valida estado do jogo
% 3. Garante existência do diretório
% 4. Cria arquivo com estado serializado
save_game(FileName, GameState) :-
    valid_save_name(FileName),
    validate_game_state(GameState), % Valida estado antes de salvar
    ensure_save_directory,
    save_directory(Dir),
    atomic_list_concat([Dir, '/', FileName, '.sav'], Path),
    (   exists_file(Path)
    ->  throw(file_already_exists(Path))
    ;   setup_call_cleanup(
            open(Path, write, Stream),
            (   write(Stream, ':- dynamic(saved_game/1).\n'),
                write(Stream, ':- multifile(saved_game/1).\n'),
                write_canonical(Stream, saved_game(GameState)), % Formato canônico
                write(Stream, '.\n'),
                flush_output(Stream)
            ),
            close(Stream)
        )
    ).

%% load_game(+FileName, -GameState)
% Carrega um jogo salvo a partir de arquivo
%
% Parâmetros:
%   FileName - Nome do arquivo a ser carregado
%   GameState - Estado do jogo carregado
%
% Fluxo:
% 1. Valida nome do arquivo
% 2. Verifica existência do arquivo
% 3. Lê e valida o estado do jogo
load_game(FileName, GameState) :-
    valid_save_name(FileName),
    save_directory(Dir),
    atomic_list_concat([Dir, '/', FileName, '.sav'], Path),
    (   exists_file(Path)
    ->  setup_call_cleanup(
            open(Path, read, Stream),
            read_saved_game(Stream, GameState),
            close(Stream)
        ),
        validate_game_state(GameState)
    ;   throw(file_not_found(Path))
    ).

%% list_saved_games(-Saves)
% Lista todos os jogos salvos disponíveis
%
% Parâmetros:
%   Saves - Lista de nomes de jogos salvos (sem extensão)
list_saved_games(Saves) :-
    ensure_save_directory,
    save_directory(Dir),
    directory_files(Dir, Files),
    include(is_valid_save_file, Files, SaveFiles),
    maplist(remove_save_extension, SaveFiles, Saves).

%% delete_saved_game(+FileName)
% Remove um jogo salvo
%
% Parâmetros:
%   FileName - Nome do arquivo a ser removido
delete_saved_game(FileName) :-
    valid_save_name(FileName),
    save_directory(Dir),
    atomic_list_concat([Dir, '/', FileName, '.sav'], Path),
    (   exists_file(Path)
    ->  delete_file(Path)
    ;   throw(file_not_found(Path))
    ).

% =============================================
% VALIDAÇÕES
% =============================================

%% valid_save_name(+Name)
% Valida se um nome é adequado para salvamento
%
% Parâmetros:
%   Name - Nome a ser validado
%
% Restrições:
% - Deve ser um átomo
% - Não pode conter barras
% - Comprimento entre 1 e 50 caracteres
valid_save_name(Name) :-
    (   atom(Name),
        \+ sub_atom(Name, _, _, _, '/'),
        \+ sub_atom(Name, _, _, _, '\\'),
        atom_length(Name, L),
        L > 0, L =< 50
    ->  true
    ;   throw(invalid_save_name(Name))
    ).


% =============================================
% AUXILIARES
% =============================================

%% ensure_save_directory/0
% Garante que o diretório de saves existe
ensure_save_directory :-
    save_directory(Dir),
    (   exists_directory(Dir)
    ->  true
    ;   make_directory_path(Dir)
    ).

%% is_valid_save_file(+File)
% Verifica se um arquivo é um save válido
%
% Parâmetros:
%   File - Nome do arquivo a verificar
is_valid_save_file(File) :-
    save_directory(Dir),
    directory_file_path(Dir, File, FullPath),
    exists_file(FullPath),
    file_name_extension(_, sav, File).

%% remove_save_extension(+File, -Name)
% Remove a extensão .sav de um nome de arquivo
%
% Parâmetros:
%   File - Nome do arquivo com extensão
%   Name - Nome sem extensão
remove_save_extension(File, Name) :-
    file_name_extension(Name, sav, File).

%% read_saved_game(+Stream, -GameState)
% Lê um estado de jogo de um stream
%
% Parâmetros:
%   Stream - Stream de entrada
%   GameState - Estado do jogo lido
read_saved_game(Stream, GameState) :-
    read_next_saved_game(Stream, GameState).

%% read_next_saved_game(+Stream, -GameState)
% Implementação recursiva da leitura de saves
read_next_saved_game(Stream, GameState) :-
    read(Stream, Term),
    (   Term == end_of_file
    ->  throw(invalid_save_format(end_of_file))
    ;   Term = saved_game(GS)
    ->  GameState = GS
    ;   read_next_saved_game(Stream, GameState)  % tenta próxima linha
    ).


%% validate_game_state(+GameState)
% Valida a estrutura do estado do jogo
%
% Parâmetros:
%   GameState - Estado do jogo a ser validado
validate_game_state(GameState) :-
    catch(
        (   GameState = game_state(CurrentGrid, Lives, _, _, (X,Y)),
            integer(Lives), Lives >= 0,
            grid_dimensions(CurrentGrid, Rows, Cols),
            MaxRow is Rows - 1,
            MaxCol is Cols - 1,
            between(0, MaxRow, X),
            between(0, MaxCol, Y)
        ),
        Error,
        throw(invalid_game_state(Error))
    ).

%% grid_dimensions(+Grid, -Rows, -Cols)
% Obtém as dimensões de uma grade
%
% Parâmetros:
%   Grid - Grade a ser medida
%   Rows - Número de linhas
%   Cols - Número de colunas
grid_dimensions(Grid, Rows, Cols) :-
    length(Grid, Rows),
    (   Rows > 0 
    ->  nth0(0, Grid, FirstRow),
        length(FirstRow, Cols)
    ;   Cols = 0
    ).