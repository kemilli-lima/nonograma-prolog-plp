% src/main.pl
:- set_prolog_flag(encoding, utf8).
:- [controller].  % Carrega o arquivo diretamente ao invés de use_module

:- initialization(run).

run :-
    start_application.  % Chama diretamente sem qualificação de módulo
