:- module(utils, [
    get_valid_input/3,
    string_to_number/2,
    pause/1,
    get_single_char/1
]).

:- use_module(library(readutil)).

get_valid_input(Min, Max, Choice) :-
    constants:error_color(Red),
    constants:reset_color(Reset),
    repeat,
    read_line_to_string(user_input, Input),
    ( catch(string_to_number(Input, Num), _, fail)
    ->  ( integer(Num),
          Num >= Min,
          Num =< Max
        -> Choice = Num, !
        ;  format('~s❌ Por favor, insira um número entre ~w e ~w.~s\n', 
                  [Red, Min, Max, Reset]),
           fail
        )
    ;   format('~s❌ Por favor, insira um número.~s\n', [Red, Reset]),
        fail
    ).

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