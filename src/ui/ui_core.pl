% Módulo principal de interface do usuário
% Responsável por toda renderização gráfica do jogo
:- module(ui_core, [
    init_ui/0,
    clear_screen/0,
    draw_title/0,
    draw_ui/1,
    show_victory/0,
    show_game_over/0,
    cleanup_systems/0,
    block_cell_row/2
]).

:- use_module(library(lists)).       % Para operações com listas
:- use_module('../constants').      % Cores e configurações
:- use_module('../core/game_state'). % Acesso ao estado do jogo
:- use_module(library(clpfd)).      % Para restrições numéricas

% Inicialização vazia
init_ui.

% Limpa o terminal usando códigos ANSI
clear_screen :-
    write('\33[2J\33[H'). % [2J limpa tela, [H move cursor para canto

% Desenha o título artístico do jogo com cores
draw_title :-
    constants:title_color(Title),    % Obtém cor do título
    constants:reset_color(Reset),    % Obtém código para resetar cor
    format("~s", [Title]),           % Aplica cor
    format("██  █  ███  ██  █  ███  █████  ███  ████ █   █ ████  ~n"),
    format("█ █ █ █   █ █ █ █ █   █ █      █  █ █  █ ██ ██ █  █~n"),
    format("█  ██ █   █ █  ██ █   █ █ ███  ███  ████ █ █ █ ████ ~n"),
    format("█   █  ███  █   █  ███  █████  █  █ █  █ █   █ █  █ ~n~n"),
    format("~s", [Reset]). % Reseta cores

% Renderiza a interface principal do jogo
draw_ui(GameState) :-
    clear_screen,
    draw_title,
    % Extrai componentes do estado do jogo
    GameState = game_state(Board, Lives, Game, _Solved, (SelRow, SelCol)),
    Game = game(_, RowHints, ColHints, _),
    
    % Mostra vidas e tabuleiro
    print_lives(Lives), nl,
    build_ui_blocks(Board, RowHints, ColHints, SelRow, SelCol, Lines),
    maplist(writeln, Lines), nl, % Imprime cada linha do tabuleiro
    print_game_menu,
    flush_output(user_output). % Garante que tudo seja exibido

% Exibe as vidas restantes com corações
print_lives(Lives) :-
    constants:error_color(ErrorColor), 
    constants:reset_color(ResetColor),
    format('~sVidas restantes: ', [ErrorColor]),
    print_hearts(Lives),
    format('~s', [ResetColor]).

print_hearts(Lives) :-
    Lives > 0,
    forall(between(1, Lives, _), write('❤️ ')).
print_hearts(_) :- write('').

% ===== Sistema de Construção do Tabuleiro =====

% Constrói todos os blocos da interface
build_ui_blocks(Board, RowHints, ColHints, SelRow, SelCol, Lines) :-
    % Prepara dicas das linhas
    maplist(clean_hint_list, RowHints, CleanedHints),
    maplist(length, CleanedHints, RowHintLengths),
    max_list(RowHintLengths, MaxHintLen),
    block_row_hints(CleanedHints, MaxHintLen, RowHintBlocks),
    
    % Prepara células do tabuleiro
    length(Board, NumRows),
    NumRowsMinus1 is NumRows - 1,
    findall(I, between(0, NumRowsMinus1, I), RowIndices),
    maplist(build_board_row(SelRow, SelCol), RowIndices, Board, BoardBlocks),
    maplist(block_cell_row, BoardBlocks, CellBlocks),
    
    % Combina tudo
    merge_row_blocks(RowHintBlocks, CellBlocks, BodyLines),
    get_row_hint_width(RowHintBlocks, Pad),
    Pad2 is Pad + 1,
    block_col_hints(ColHints, Pad2, ColHintLines),
    append(ColHintLines, BodyLines, Lines).

% Limpa zeros das dicas (0 → "")
clean_hint_list([0], ["0"]) :- !.
clean_hint_list(Hints, Cleaned) :-
    exclude(==(0), Hints, NoZeros),
    maplist(number_string, NoZeros, Strs),
    ( Strs = [] -> Cleaned = [""] ; Cleaned = Strs ).

% Constrói blocos de dicas para linhas
block_row_hints([], _, []).
block_row_hints([Hints|Rest], MaxHints, [LineBlock|RestLines]) :-
    length(Hints, Len),
    Pad is MaxHints - Len,
    length(Prefix, Pad), % Padding para alinhamento
    maplist(=(""), Prefix),
    append(Prefix, Hints, FullHints),
    maplist(format_hint, FullHints, HintStrs),
    atomic_list_concat(HintStrs, '', HStr),
    format(string(Line), '~w ', [HStr]),
    LineBlock = [Line],
    block_row_hints(Rest, MaxHints, RestLines).

% Formata dicas com padding fixo (4 caracteres)
format_hint("", "    "). % Espaços vazios
format_hint(H, Fmt) :- 
    ( H = "0" -> Fmt = "    " % Trata 0 como vazio
    ; format(string(Fmt), "~|~t~w~4+", [H]) % Alinha à direita
    ).

% Constrói uma linha do tabuleiro
build_board_row(SelRow, SelCol, RowIdx, Row, BlockRow) :-
    length(Row, NumCols),
    NumColsMinus1 is NumCols - 1,
    findall(I, between(0, NumColsMinus1, I), ColIndices),
    maplist(build_cell(RowIdx, SelRow, SelCol), ColIndices, Row, BlockRow).

% Constrói uma célula individual
build_cell(RIdx, SRow, SCol, CIdx, Cell, Block) :-
    (RIdx =:= SRow, CIdx =:= SCol -> Selected = true ; Selected = false),
    block_cell(Cell, Selected, Block).

% Representação visual das células:
% Não selecionadas
block_cell(filled, false, [" 🟧 "]).  % Preenchida
block_cell(marked, false, [" ❌ "]).  % Marcada
block_cell(empty, false,  [" ·  "]). % Vazia
% Selecionadas (com [ ])
block_cell(filled, true,  ["[🟧]"]). 
block_cell(marked, true,  ["[❌]"]).
block_cell(empty, true,   ["[· ]"]).

block_cell_row(Cells, BlockRow) :-
    maplist(nth0(0), Cells, BlockRow).

% Combina blocos horizontais
merge_row_blocks([], [], []).
merge_row_blocks([H1|T1], [H2|T2], [Line|Rest]) :-
    append(H1, H2, Row),
    atomic_list_concat(Row, '', Line),
    merge_row_blocks(T1, T2, Rest).

% ===== Dicas das Colunas =====
block_col_hints(Hints, RowHintWidth, Lines) :-
    maplist(clean_hint_list_col, Hints, HintsStr),
    max_column_height(HintsStr, MaxHeight),
    pad_column_hints(HintsStr, MaxHeight, Padded),
    transpose(Padded, Rows), % Transpõe para exibição vertical
    maplist(format_col_hint_line, Rows, RawLines),
    format_padding(RowHintWidth, Padding),
    maplist({Padding}/[L,Out]>>string_concat(Padding,L,Out), RawLines, Lines).

% Formatação do padding esquerdo
format_padding(Width, Padding) :-
    length(SpaceList, Width),
    maplist(=(' '), SpaceList),
    atomic_list_concat(SpaceList, '', Padding).

% Limpa dicas das colunas
clean_hint_list_col(Hints, Cleaned) :-
    exclude(==(0), Hints, NoZeros),
    maplist(number_string, NoZeros, Cleaned).

% Formata linha de dicas das colunas
format_col_hint_line(Hints, Line) :-
    maplist(pad_hint, Hints, Final),
    atomic_list_concat(Final, '', Line).

% Padding para dicas das colunas
pad_hint(Hint, Out) :-
    format(string(Out), "~|~w~t~4+", [Hint]).

% Adiciona padding vertical às dicas
pad_column_hints([], _, []).
pad_column_hints([H|T], Max, [Padded|Rest]) :-
    length(H, Len),
    Pad is Max - Len,
    length(Suffix, Pad),
    maplist(=(""), Suffix),
    append(Suffix, H, Padded),
    pad_column_hints(T, Max, Rest).

% Calcula altura máxima das dicas
max_column_height(Hints, Max) :-
    maplist(length, Hints, Lengths),
    max_list(Lengths, Max).

% Calcula largura das dicas das linhas
get_row_hint_width([], 0).
get_row_hint_width([[Str]|T], MaxWidth) :-
    string_length(Str, Len),
    get_row_hint_width(T, Rest),
    max_list([Len, Rest], MaxWidth).

% Menu de controles
print_game_menu :-
    write('[w/a/s/d] mover | [f] preencher | [m] marcar | [h] dica | [q] sair'), nl.

% ===== Telas de Fim de Jogo =====
show_victory :-
    clear_screen,
    write('\n🎉 Você venceu! Parabéns! 🎉\n').

show_game_over :-
    clear_screen,
    write('\n☠️  Game Over! Tente novamente.\n').

% Limpeza ao sair
cleanup_systems :-
    format("~sSistema encerrado com segurança.~n", [constants:title_color]),
    constants:reset_color, nl.