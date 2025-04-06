:- module(puzzle_loader, [
    load_puzzle/2,                 % Carrega um puzzle aleatório com base na dificuldade
    validate_game/1,              % Valida a estrutura de um puzzle carregado
    list_available_puzzles/1      % Lista os puzzles disponíveis no diretório
]).

:- use_module(library(lists)).
:- use_module(library(random)).

file_path(Difficulty, Path) :-    
    atomic_list_concat(['data/puzzles/', Difficulty, '.pl'], Path).  % Monta o caminho do arquivo com base na dificuldade

load_puzzle(Difficulty, Game) :-
    file_path(Difficulty, Path),
    (   exists_file(Path)
    ->  use_module(Path),                              % Carrega dinamicamente o módulo do puzzle
        module_name(Path, Module),                     
        Module:puzzles(PuzzleList),                    % Obtém a lista de puzzles do módulo
        length(PuzzleList, Len),
        (   Len > 0
        ->  random_between(1, Len, Index),             % Escolhe um puzzle aleatoriamente
            nth1(Index, PuzzleList, puzzle(Solution, RowHints, ColHints, Difficulty)),
            Game = game(Solution, RowHints, ColHints, Difficulty)
        ;   throw(error(no_puzzles_found(Difficulty), _))
        ),
        (   validate_game(Game)                         % Valida o puzzle carregado
        ->  true
        ;   format("VALIDAÇÃO FALHOU PARA GAME: ~w~n", [Game]),
            throw(error(invalid_puzzle_format(Path), _))
        )
    ;   throw(error(file_not_found(Path), _))          % Arquivo de puzzles não encontrado
    ).

module_name(File, Module) :-                            
    file_base_name(File, Base),
    file_name_extension(Module, _, Base).              % Extrai o nome do módulo a partir do nome do arquivo

%% --- Validação do Puzzle ---

validate_game(game(Solution, RowsHints, ColsHints, _)) :-
    length(Solution, NumRows),
    length(RowsHints, NumRows),                        % Número de linhas deve bater com as dicas das linhas
    (   NumRows > 0
    ->  nth0(0, Solution, FirstRow),
        length(FirstRow, NumCols),
        length(ColsHints, NumCols)                     % Número de colunas deve bater com as dicas das colunas
    ;   true
    ),
    maplist(normalize_hint, RowsHints, NormalizedRowHints),
    maplist(normalize_hint, ColsHints, NormalizedColHints),
    maplist(validate_line, Solution, NormalizedRowHints),   % Valida se as dicas batem com as linhas
    transpose(Solution, Transposed),
    maplist(validate_line, Transposed, NormalizedColHints). % Valida se as dicas batem com as colunas

normalize_hint([0], []) :- !.                          % Transforma dica [0] em []
normalize_hint(Hints, Hints).                          % Deixa outras dicas como estão

validate_line(Line, ExpectedHints) :-
    calculate_hints(Line, ActualHints),
    (   ExpectedHints == ActualHints                   % Compara dicas esperadas com calculadas
    ->  true
    ;   format("\n⚠️  Validação falhou para linha: ~w~n", [Line]),
        format("    Esperado: ~w~n", [ExpectedHints]),
        format("    Obtido  : ~w~n", [ActualHints]),
        fail
    ).

%% Gera as dicas (sequências de "filled") de uma linha da grade
calculate_hints(Row, Hints) :-
    calculate_hints(Row, 0, [], RevHints),
    reverse(RevHints, Hints).

calculate_hints([], 0, Hints, Hints).
calculate_hints([], Current, Hints, [Current|Hints]) :-
    Current > 0.
calculate_hints([filled|Rest], Current, Acc, Hints) :-
    NewCurrent is Current + 1,
    calculate_hints(Rest, NewCurrent, Acc, Hints).
calculate_hints([marked|Rest], 0, Acc, Hints) :-
    calculate_hints(Rest, 0, Acc, Hints).
calculate_hints([marked|Rest], Current, Acc, Hints) :-
    Current > 0,
    calculate_hints(Rest, 0, [Current|Acc], Hints).

%% Lista os arquivos de puzzles disponíveis no diretório 'data/puzzles'
list_available_puzzles(Puzzles) :-
    directory_files('data/puzzles', Files),
    include(is_puzzle_file, Files, PuzzleFiles),
    maplist(extract_difficulty, PuzzleFiles, Puzzles).

is_puzzle_file(File) :-
    file_name_extension(_, pl, File).                  % Arquivo termina com .pl

extract_difficulty(File, Difficulty) :-
    file_name_extension(Difficulty, pl, File).         % Extrai o nome base do arquivo como dificuldade

%% Implementação de transpose/2 caso não esteja disponível
:- if(\+current_predicate(transpose/2)).
transpose([], []).
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Rows]) :-
    maplist(list_head_tail, Matrix, Row, Tails),
    transpose(Tails, Rows).

list_head_tail([H|T], H, T).
:- endif.
