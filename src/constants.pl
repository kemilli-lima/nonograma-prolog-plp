% constants.pl
:- module(constants, [
    ui_width/1,
    title_color/1,
    option_color/1,
    success_color/1,
    warning_color/1,
    error_color/1,
    reset_color/1,
    blue_color/1,
    magenta_color/1
]).

ui_width(50).

title_color("\033[1;36m").     % Ciano brilhante
option_color("\033[0;37m").    % Branco
success_color("\033[1;32m").   % Verde brilhante
warning_color("\033[1;33m").   % Amarelo/Laranja brilhante
error_color("\033[1;31m").     % Vermelho brilhante
reset_color("\033[0m").        % Reset
blue_color("\033[1;34m").      % Azul brilhante
magenta_color("\033[1;35m").   % Magenta brilhante
