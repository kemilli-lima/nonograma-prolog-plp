% Módulo utils.pl - Utilitários Gerais do Nonograma
%
% Responsável por operações auxiliares utilizadas em diversos módulos:
% - Validação de entrada do usuário
% - Conversão de tipos
% - Controle de fluxo/tempo
% - Manipulação de caracteres

:- module(utils, [
    get_valid_input/3,      % Obtém e valida entrada numérica
    string_to_number/2,     % Converte string para número
    pause/1,                % Pausa a execução por N segundos
    get_single_char/1       % Captura um único caractere
]).

% Dependências
:- use_module(library(readutil)).   % Para leitura de strings

% =============================================
% VALIDAÇÃO DE ENTRADA
% =============================================

%% get_valid_input(+Min, +Max, -Choice)
% Obtém e valida entrada do usuário dentro de um intervalo
%
% Parâmetros:
%   Min - Valor mínimo aceitável (inclusive)
%   Max - Valor máximo aceitável (inclusive)
%   Choice - Valor validado retornado
%
% Fluxo:
% 1. Solicita entrada do usuário
% 2. Tenta converter para número
% 3. Verifica se está no intervalo [Min, Max]
% 4. Repete até obter entrada válida
%
% Exibe mensagens de erro coloridas quando:
% - Entrada não é numérica
% - Número fora do intervalo permitido
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

% =============================================
% CONVERSÃO DE TIPOS
% =============================================

%% string_to_number(+Str, -Num)
% Converte string/átomo para número de forma segura
%
% Parâmetros:
%   Str - String ou átomo contendo número
%   Num - Número resultante
%
% Retorna:
%   true se conversão bem-sucedida
%   fail se não for possível converter
string_to_number(Str, Num) :-
    catch(atom_number(Str, Num), _, fail).

% =============================================
% CONTROLE DE TEMPO
% =============================================

%% pause(+Secs)
% Pausa a execução por N segundos
%
% Parâmetros:
%   Secs - Número de segundos para pausar
%
% Uso típico:
% - Exibição temporária de mensagens
% - Controle de velocidade de animações
pause(Secs) :-
    sleep(Secs).

% =============================================
% MANIPULAÇÃO DE ENTRADA
% =============================================

%% get_single_char(-Char)
% Captura um único caractere do usuário (ignora ENTER)
%
% Parâmetros:
%   Char - Caractere capturado
%
% Fluxo:
% 1. Lê caractere da entrada
% 2. Se for ENTER, lê novamente
% 3. Retorna primeiro caractere não-ENTER
get_single_char(Char) :-
    get_char(RawChar),
    ( RawChar = '\n' -> 
        get_single_char(Char)
    ; 
        Char = RawChar
    ).