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

save_directory('data/saves').

%% save_game(+FileName, +GameState)
save_game(FileName, GameState) :-
    valid_save_name(FileName),
    ensure_save_directory,
    save_directory(Dir),
    atomic_list_concat([Dir, '/', FileName, '.sav'], Path),
    (   exists_file(Path)
    ->  throw(file_already_exists(Path))
    ;   setup_call_cleanup(
            open(Path, write, Stream),
            (   write(Stream, ':- dynamic(saved_game/1).\n'),
                write(Stream, ':- multifile(saved_game/1).\n'),
                write_canonical(Stream, saved_game(GameState)),
                write(Stream, '.\n'),
                flush_output(Stream)
            ),
            close(Stream)
        ),
        validate_saved_game(Path)
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
    delete_file(Path).

%% valid_save_name(+Name)
valid_save_name(Name) :-
    atom(Name),
    \+ sub_atom(Name, _, _, _, '/'),
    \+ sub_atom(Name, _, _, _, '\\'),
    atom_length(Name, L),
    L > 0, L =< 50.

%% ensure_save_directory/0
ensure_save_directory :-
    save_directory(Dir),
    (   exists_directory(Dir)
    ->  true
    ;   make_directory_path(Dir)
    ).

is_valid_save_file(File) :-
    file_name_extension(_, sav, File).

remove_save_extension(File, Name) :-
    file_name_extension(Name, sav, File).

read_saved_game(Stream, GameState) :-
    read(Stream, saved_game(GameState)),
    !.
read_saved_game(_, _) :-
    throw(invalid_save_format).

validate_saved_game(Path) :-
    load_game(Path, GameState),
    validate_game_state(GameState).

validate_game_state(GameState) :-
    catch((
        GameState = game_state(CurrentGrid, _Lives, _Game, _IsSolved, _),
        game_state:valid_coordinates(CurrentGrid, _X, _Y)
    ), Error, throw(invalid_game_state(Error))).

