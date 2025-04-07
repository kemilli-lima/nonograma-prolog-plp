% Módulo ui_core.pl - Sistema de Interface do Usuário
%
% Responsável por:
% - Renderização gráfica do jogo
% - Exibição de menus e telas
% - Formatação de elementos visuais
% - Gerenciamento de cores e estilos

:- module(ui_core, [
    init_ui/0,              % Inicializa sistema de UI
    clear_screen/0,         % Limpa a tela do terminal
    draw_title/0,           % Exibe título artístico do jogo
    draw_ui/1,             % Renderiza interface completa do jogo
    show_victory/1,        % Mostra tela de vitória
    show_game_over/1,      % Mostra tela de game over
    cleanup_systems/0,     % Finaliza sistemas de UI
    block_cell_row/2       % Auxiliar para renderização de linhas
]).

:- use_module(library(lists)).      % Para operações com listas
:- use_module('../constants').      % Para constantes de cores
:- use_module('../core/game_state'). % Para manipulação do estado do jogo
:- use_module(library(clpfd)).      % Para restrições lógicas
:- use_module('../utils').          % Para utilitários


% =============================================
% INICIALIZAÇÃO E FINALIZAÇÃO
% =============================================

%% init_ui
% Inicializa o sistema de interface do usuário
init_ui.

% =============================================
% CONTROLE DE TELA
% =============================================

%% clear_screen
% Limpa o terminal usando sequências de escape ANSI
clear_screen :-
    write('\33[2J\33[H').

%% draw_title
% Exibe o título artístico do jogo em ASCII com cores
draw_title :-
    constants:title_color(Title),
    constants:reset_color(Reset),
    format("~s", [Title]),
    format("██  █  ███  ██  █  ███  █████  ███  ████ █   █ ████  ~n"),
    format("█ █ █ █   █ █ █ █ █   █ █      █  █ █  █ ██ ██ █  █~n"),
    format("█  ██ █   █ █  ██ █   █ █ ███  ███  ████ █ █ █ ████ ~n"),
    format("█   █  ███  █   █  ███  █████  █  █ █  █ █   █ █  █ ~n~n"),
    format("~s", [Reset]).

% =============================================
% RENDERIZAÇÃO PRINCIPAL
% =============================================

%% draw_ui(+GameState)
% Renderiza a interface completa do jogo incluindo:
% - Tabuleiro com estado atual
% - Dicas de linhas/colunas
% - Cursor de seleção
% - Contador de vidas
% - Menu de controles
draw_ui(GameState) :-
    clear_screen,
    draw_title,
    GameState = game_state(Board, Lives, Game, _Solved, (SelRow, SelCol)),
    Game = game(_, RowHints, ColHints, _),
    
    print_lives(Lives), nl,
    build_ui_blocks(Board, RowHints, ColHints, SelRow, SelCol, Lines),
    maplist(writeln, Lines), nl,
    print_game_menu, 
    flush_output(user_output).

%% print_lives(+Lives)
% Exibe contador de vidas com ícones de coração
print_lives(Lives) :-
    constants:error_color(ErrorColor), 
    constants:reset_color(ResetColor),
    format('~sVidas restantes: ', [ErrorColor]),
    print_hearts(Lives),
    format('~s', [ResetColor]).

%% print_hearts(+Lives)
% Auxiliar para exibir ícones de coração conforme vidas restantes
print_hearts(Lives) :-
    Lives > 0,
    forall(between(1, Lives, _), write('❤️ ')).
print_hearts(_) :- write('').

% =============================================
% CONSTRUÇÃO DE BLOCOS VISUAIS
% =============================================

%% build_ui_blocks(+Board, +RowHints, +ColHints, +SelRow, +SelCol, -Lines)
% Constrói a representação visual completa do tabuleiro:
% - Dicas de coluna no topo
% - Dicas de linha à esquerda
% - Células do tabuleiro
% - Destaque para célula selecionada
build_ui_blocks(Board, RowHints, ColHints, SelRow, SelCol, Lines) :-
    maplist(clean_hint_list, RowHints, CleanedHints),
    maplist(length, CleanedHints, RowHintLengths),
    max_list(RowHintLengths, MaxHintLen),
    block_row_hints(CleanedHints, MaxHintLen, RowHintBlocks),
    
    length(Board, NumRows),
    NumRowsMinus1 is NumRows - 1,
    findall(I, between(0, NumRowsMinus1, I), RowIndices),
    maplist(build_board_row(SelRow, SelCol), RowIndices, Board, BoardBlocks),
    maplist(block_cell_row, BoardBlocks, CellBlocks),
    
    merge_row_blocks(RowHintBlocks, CellBlocks, BodyLines),
    get_row_hint_width(RowHintBlocks, Pad),
    Pad2 is Pad + 1,
    block_col_hints(ColHints, Pad2, ColHintLines),
    append(ColHintLines, BodyLines, Lines).

%% clean_hint_list(+Hints, -Cleaned)
% Normaliza dicas removendo zeros desnecessários
clean_hint_list([0], ["0"]) :- !.
clean_hint_list(Hints, Cleaned) :-
    exclude(==(0), Hints, NoZeros),
    maplist(number_string, NoZeros, Strs),
    ( Strs = [] -> Cleaned = [""] ; Cleaned = Strs ).


% =============================================
% RENDERIZAÇÃO DE DICAS
% =============================================

%% block_row_hints(+HintsList, +MaxHints, -HintBlocks)
% Formata dicas de linha com alinhamento consistente
block_row_hints([], _, []).
block_row_hints([Hints|Rest], MaxHints, [LineBlock|RestLines]) :-
    length(Hints, Len),
    Pad is MaxHints - Len,
    length(Prefix, Pad),
    maplist(=(""), Prefix),
    append(Prefix, Hints, FullHints),
    maplist(format_hint, FullHints, HintStrs),
    atomic_list_concat(HintStrs, '', HStr),
    format(string(Line), '~w ', [HStr]),
    LineBlock = [Line],
    block_row_hints(Rest, MaxHints, RestLines).

%% format_hint(+Hint, -Formatted)
% Formata dicas individuais com padding consistente
format_hint("", "    ").
format_hint(H, Fmt) :- 
    ( H = "0" -> Fmt = "    "
    ; format(string(Fmt), "~|~t~w~4+", [H])
    ).

% =============================================
% RENDERIZAÇÃO DO TABULEIRO
% =============================================

%% build_board_row(+SelRow, +SelCol, +RowIdx, +Row, -BlockRow)
% Constrói representação visual de uma linha do tabuleiro
build_board_row(SelRow, SelCol, RowIdx, Row, BlockRow) :-
    length(Row, NumCols),
    NumColsMinus1 is NumCols - 1,
    findall(I, between(0, NumColsMinus1, I), ColIndices),
    maplist(build_cell(RowIdx, SelRow, SelCol), ColIndices, Row, BlockRow).

%% build_cell(+RIdx, +SRow, +SCol, +CIdx, +Cell, -Block)
% Constrói representação visual de uma célula individual
build_cell(RIdx, SRow, SCol, CIdx, Cell, Block) :-
    (RIdx =:= SRow, CIdx =:= SCol -> Selected = true ; Selected = false),
    block_cell(Cell, Selected, Block).

%% block_cell(+CellState, +Selected, -Block)
% Mapeia estados de célula para representação visual
block_cell(filled, false, [" 🟧 "]).  % Célula preenchida
block_cell(marked, false, [" ❌ "]).  % Célula marcada como vazia
block_cell(empty, false,  [" ·  "]).  % Célula vazia
block_cell(filled, true,  ["[🟧]"]). % Célula selecionada
block_cell(marked, true,  ["[❌]"]). % Célula selecionada
block_cell(empty, true,   ["[· ]"]). % Célula selecionada

% =============================================
% MANIPULAÇÃO DE LINHAS E COLUNAS DO TABULEIRO
% =============================================

%% block_cell_row(+Cells, -BlockRow)
% Converte uma linha de células em sua representação textual
% 
% Parâmetros:
%   Cells    - Lista de blocos de células (cada célula é uma lista com um elemento)
%   BlockRow - Lista de strings prontas para exibição
%
% Funcionamento:
%   Extrai o primeiro elemento de cada bloco de célula usando nth0/3
block_cell_row(Cells, BlockRow) :-
    maplist(nth0(0), Cells, BlockRow).

%% merge_row_blocks(+RowHints, +CellBlocks, -MergedLines)
% Combina blocos de dicas de linha com blocos de células em linhas completas
%
% Parâmetros:
%   RowHints   - Lista de dicas de linha formatadas
%   CellBlocks - Lista de blocos de células formatadas
%   MergedLines- Linhas completas prontas para exibição
%
% Funcionamento:
%   Concatena as dicas de linha com as células correspondentes
%   e junta os elementos em uma única string por linha
merge_row_blocks([], [], []).
merge_row_blocks([H1|T1], [H2|T2], [Line|Rest]) :-
    append(H1, H2, Row),                        % Combina dicas e células
    atomic_list_concat(Row, '', Line),          % Junta elementos em string
    merge_row_blocks(T1, T2, Rest).             % Processa recursivamente

%% block_col_hints(+Hints, +RowHintWidth, -Lines)
% Formata as dicas de coluna para exibição acima do tabuleiro
%
% Parâmetros:
%   Hints        - Lista de dicas de coluna originais
%   RowHintWidth - Largura necessária para alinhamento com as dicas de linha
%   Lines        - Linhas formatadas das dicas de coluna
%
% Fluxo:
% 1. Limpa e converte dicas para strings
% 2. Calcula altura máxima das colunas
% 3. Adiciona padding para uniformizar altura
% 4. Transpõe matriz para exibição horizontal
% 5. Formata cada linha com padding de alinhamento
block_col_hints(Hints, RowHintWidth, Lines) :-
    maplist(clean_hint_list_col, Hints, HintsStr),  % Limpa dicas
    max_column_height(HintsStr, MaxHeight),         % Calcula altura máxima
    pad_column_hints(HintsStr, MaxHeight, Padded),  % Adiciona padding
    transpose(Padded, Rows),                        % Transpõe matriz
    maplist(format_col_hint_line, Rows, RawLines),  % Formata linhas
    format_padding(RowHintWidth, Padding),          % Cria padding para alinhamento
    % Aplica padding a cada linha usando lambda
    maplist({Padding}/[L,Out]>>string_concat(Padding,L,Out), RawLines, Lines).

%% format_padding(+Width, -Padding)
% Cria uma string de espaços para alinhamento
%
% Parâmetros:
%   Width   - Número de espaços a serem criados
%   Padding - String resultante com espaços
format_padding(Width, Padding) :-
    length(SpaceList, Width),                   % Cria lista de espaços
    maplist(=(' '), SpaceList),                 % Garante que são espaços
    atomic_list_concat(SpaceList, '', Padding). % Converte para string

%% clean_hint_list_col(+Hints, -Cleaned)
% Filtra e converte dicas de coluna numéricas para strings
%
% Parâmetros:
%   Hints   - Lista de números representando dicas
%   Cleaned - Lista de strings limpas (sem zeros)
clean_hint_list_col(Hints, Cleaned) :-
    exclude(==(0), Hints, NoZeros),             % Remove zeros
    maplist(number_string, NoZeros, Cleaned).   % Converte para strings

%% format_col_hint_line(+Hints, -Line)
% Formata uma linha de dicas de coluna com padding consistente
%
% Parâmetros:
%   Hints - Lista de strings de dicas
%   Line  - String formatada com padding uniforme
format_col_hint_line(Hints, Line) :-
    maplist(pad_hint, Hints, Final),        % Aplica padding a cada dica
    atomic_list_concat(Final, '', Line).    % Junta em uma string

%% pad_hint(+Hint, -Out)
% Formata uma dica individual com padding fixo de 4 caracteres
%
% Parâmetros:
%   Hint - String com a dica original
%   Out  - String formatada com padding
pad_hint(Hint, Out) :-
    format(string(Out), "~|~w~t~4+", [Hint]).   % Formata com 4 caracteres de largura

%% pad_column_hints(+Hints, +Max, -Padded)
% Adiciona padding vertical às colunas de dicas para uniformizar altura
%
% Parâmetros:
%   Hints  - Lista de listas de dicas por coluna
%   Max    - Altura máxima desejada
%   Padded - Lista com colunas preenchidas com strings vazias
pad_column_hints([], _, []).
pad_column_hints([H|T], Max, [Padded|Rest]) :-
    length(H, Len),
    Pad is Max - Len,                       % Calcula padding necessário
    length(Suffix, Pad),                    % Cria lista de padding
    maplist(=(""), Suffix),                 % Preenche com strings vazias
    append(Suffix, H, Padded),              % Adiciona padding no início
    pad_column_hints(T, Max, Rest).         % Processa recursivamente

%% max_column_height(+Hints, -Max)
% Calcula a altura máxima entre todas as colunas de dicas
%
% Parâmetros:
%   Hints - Lista de listas de dicas por coluna
%   Max   - Maior comprimento encontrado
max_column_height(Hints, Max) :-
    maplist(length, Hints, Lengths),    % Obtém comprimento de cada coluna
    max_list(Lengths, Max).             % Encontra o valor máximo

%% get_row_hint_width(+HintBlocks, -MaxWidth)
% Calcula a largura máxima das dicas de linha para alinhamento
%
% Parâmetros:
%   HintBlocks - Lista de blocos de dicas de linha
%   MaxWidth   - Maior largura encontrada
get_row_hint_width([], 0).
get_row_hint_width([[Str]|T], MaxWidth) :-
    string_length(Str, Len),                % Obtémcomprimento da string
    get_row_hint_width(T, Rest),            % Calcula recursivamente
    max_list([Len, Rest], MaxWidth).        % Determina o máximo

% =============================================
% MENU DO JOGO
% =============================================

%% print_game_menu
% Exibe o menu de controles do jogo com formatação colorida
print_game_menu :-
    constants:title_color(TitleColor),
    constants:blue_color(BlueColor),
    constants:warning_color(WarningColor),
    constants:magenta_color(MagentaColor),
    constants:success_color(SuccessColor),
    constants:reset_color(Reset),
    constants:error_color(RedColor),
    

    format("~s╔════════════════════════════════════════════════════╗~n", [TitleColor]),
    format("~s║              ◆ ESCOLHA UMA OPÇÃO ◆                 ║~n", [TitleColor]),
    format("~s╠════════════════════════════════════════════════════╣~n", [TitleColor]),

    format("~s║ ➤  Mover célula  →  teclas w / a / s / d           ║~n", [BlueColor]),
    format("~s", [Reset]),

    format("~s║ ➤  Preencher célula  →  tecla f                    ║~n", [WarningColor]),
    format("~s", [Reset]),

    format("~s║ ➤  Marcar célula  →  tecla m                       ║~n", [MagentaColor]),
    format("~s", [Reset]),

    format("~s║ ➤  Pedir dica  →  tecla h                          ║~n", [RedColor]),
    format("~s", [Reset]),

    format("~s║ ➤  Sair  →  tecla q                                ║~n", [Reset]),
    format("~s", [Reset]),

    format("~s║ ➤  Salvar jogo (Use a tecla v)                     ║~n", [SuccessColor]),
    format("~s", [Reset]),

    format("~s╚════════════════════════════════════════════════════╝~n~n", [TitleColor]),
    format("~s", [Reset]).

% =============================================
% TELAS DE FIM DE JOGO
% =============================================

%% show_victory(+GameState)
% Exibe tela de vitória mantendo o estado final do tabuleiro
show_victory(GameState) :-
    draw_ui(GameState),
    write('\n🎉 Você venceu! Parabéns! 🎉\n').

%% show_game_over(+GameState)
% Exibe tela de game over mantendo o estado final do tabuleiro
show_game_over(GameState) :-
    draw_ui(GameState),
    write('\n☠️  Game Over! Tente novamente.\n').

% =============================================
% FINALIZAÇÃO
% =============================================

%% cleanup_systems
% Realiza limpeza final dos sistemas de UI
cleanup_systems :-
    format("~sSistema encerrado com segurança.~n", [constants:title_color]),
    constants:reset_color, nl.