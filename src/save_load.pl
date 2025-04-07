:- module(save_load, [
    save_game/2,
    load_game/2,
    list_saved_games/1,
    delete_saved_game/1,
    valid_save_name/1,
    ensure_save_directory/0
]).

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module('core/game_state').

:- dynamic game_state:valid_coordinates/3.

% Diretório padrão para salvamento
save_directory('data/saves').

%% save_game(+FileName, +GameState)
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
list_saved_games(Saves) :-
    ensure_save_directory,
    save_directory(Dir),
    directory_files(Dir, Files),
    include(is_valid_save_file, Files, SaveFiles),
    maplist(remove_save_extension, SaveFiles, Saves).

%% delete_saved_game(+FileName)
delete_saved_game(FileName) :-
    valid_save_name(FileName),
    save_directory(Dir),
    atomic_list_concat([Dir, '/', FileName, '.sav'], Path),
    (   exists_file(Path)
    ->  delete_file(Path)
    ;   throw(file_not_found(Path))
    ).

%% valid_save_name(+Name)
valid_save_name(Name) :-
    (   atom(Name),
        \+ sub_atom(Name, _, _, _, '/'),
        \+ sub_atom(Name, _, _, _, '\\'),
        atom_length(Name, L),
        L > 0, L =< 50
    ->  true
    ;   throw(invalid_save_name(Name))
    ).


%% ensure_save_directory/0
ensure_save_directory :-
    save_directory(Dir),
    (   exists_directory(Dir)
    ->  true
    ;   make_directory_path(Dir)
    ).

%% is_valid_save_file(+File)
is_valid_save_file(File) :-
    save_directory(Dir),
    directory_file_path(Dir, File, FullPath),
    exists_file(FullPath),
    file_name_extension(_, sav, File).

%% remove_save_extension(+File, -Name)
remove_save_extension(File, Name) :-
    file_name_extension(Name, sav, File).

%% read_saved_game(+Stream, -GameState)
read_saved_game(Stream, GameState) :-
    read_next_saved_game(Stream, GameState).

read_next_saved_game(Stream, GameState) :-
    read(Stream, Term),
    (   Term == end_of_file
    ->  throw(invalid_save_format(end_of_file))
    ;   Term = saved_game(GS)
    ->  GameState = GS
    ;   read_next_saved_game(Stream, GameState)  % tenta próxima linha
    ).


%% validate_game_state(+GameState)
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


% Utilitário para obter dimensões da grade
grid_dimensions(Grid, Rows, Cols) :-
    length(Grid, Rows),
    (   Rows > 0 
    ->  nth0(0, Grid, FirstRow),
        length(FirstRow, Cols)
    ;   Cols = 0
    ).