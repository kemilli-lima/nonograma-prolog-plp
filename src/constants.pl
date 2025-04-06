% constants.pl
:- module(constants, [
    ui_width/1,
    title_color/1,
    option_color/1,
    error_color/1,
    reset_color/1
]).

ui_width(50).

title_color("\033[1;36m").    % Ciano brilhante
option_color("\033[0;37m").   % Branco
error_color("\033[1;31m").    % Vermelho
reset_color("\033[0m").       % Reseta a formatação
