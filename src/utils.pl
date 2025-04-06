% src/utils.pl
:- module(utils, [
    get_valid_input/3,
    string_to_number/2,
    pause/1,
    get_single_char/1
]).

:- use_module(library(readutil)).

get_valid_input(Min, Max, Choice) :-
    repeat,
    read_line_to_string(user_input, Input),
    string_to_number(Input, Num),
    integer(Num),
    Num >= Min, Num =< Max,
    Choice = Num, !.
get_valid_input(_, _, _) :-
    write('Entrada inválida. Tente novamente.'), fail.

string_to_number(Str, Num) :-
    catch(atom_number(Str, Num), _, fail).

pause(Secs) :-
    sleep(Secs).

get_single_char(Char) :-
    get_char(RawChar),
    ( RawChar = '\n' -> 
        get_single_char(Char)
    ; 
        Char = RawChar
    ).