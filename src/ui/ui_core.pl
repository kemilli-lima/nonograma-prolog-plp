:- module(ui_core, [
    init_ui/0,
    clear_screen/0,
    draw_title/0,
    draw_ui/1,
    show_victory/1,
    show_game_over/1,
    cleanup_systems/0,
    block_cell_row/2
]).

:- use_module(library(lists)).
:- use_module('../constants').
:- use_module('../core/game_state').
:- use_module(library(clpfd)).
:- use_module('../utils').

init_ui.

clear_screen :-
    write('\33[2J\33[H').

draw_title :-
    constants:title_color(Title),
    constants:reset_color(Reset),
    format("~s", [Title]),
    format("██  █  ███  ██  █  ███  █████  ███  ████ █   █ ████  ~n"),
    format("█ █ █ █   █ █ █ █ █   █ █      █  █ █  █ ██ ██ █  █~n"),
    format("█  ██ █   █ █  ██ █   █ █ ███  ███  ████ █ █ █ ████ ~n"),
    format("█   █  ███  █   █  ███  █████  █  █ █  █ █   █ █  █ ~n~n"),
    format("~s", [Reset]).

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

clean_hint_list([0], ["0"]) :- !.
clean_hint_list(Hints, Cleaned) :-
    exclude(==(0), Hints, NoZeros),
    maplist(number_string, NoZeros, Strs),
    ( Strs = [] -> Cleaned = [""] ; Cleaned = Strs ).

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

format_hint("", "    ").
format_hint(H, Fmt) :- 
    ( H = "0" -> Fmt = "    "
    ; format(string(Fmt), "~|~t~w~4+", [H])
    ).

build_board_row(SelRow, SelCol, RowIdx, Row, BlockRow) :-
    length(Row, NumCols),
    NumColsMinus1 is NumCols - 1,
    findall(I, between(0, NumColsMinus1, I), ColIndices),
    maplist(build_cell(RowIdx, SelRow, SelCol), ColIndices, Row, BlockRow).

build_cell(RIdx, SRow, SCol, CIdx, Cell, Block) :-
    (RIdx =:= SRow, CIdx =:= SCol -> Selected = true ; Selected = false),
    block_cell(Cell, Selected, Block).

block_cell(filled, false, [" 🟧 "]).
block_cell(marked, false, [" ❌ "]).
block_cell(empty, false,  [" ·  "]).
block_cell(filled, true,  ["[🟧]"]).
block_cell(marked, true,  ["[❌]"]).
block_cell(empty, true,   ["[· ]"]).

block_cell_row(Cells, BlockRow) :-
    maplist(nth0(0), Cells, BlockRow).

merge_row_blocks([], [], []).
merge_row_blocks([H1|T1], [H2|T2], [Line|Rest]) :-
    append(H1, H2, Row),
    atomic_list_concat(Row, '', Line),
    merge_row_blocks(T1, T2, Rest).

block_col_hints(Hints, RowHintWidth, Lines) :-
    maplist(clean_hint_list_col, Hints, HintsStr),
    max_column_height(HintsStr, MaxHeight),
    pad_column_hints(HintsStr, MaxHeight, Padded),
    transpose(Padded, Rows),
    maplist(format_col_hint_line, Rows, RawLines),
    format_padding(RowHintWidth, Padding),
    maplist({Padding}/[L,Out]>>string_concat(Padding,L,Out), RawLines, Lines).

format_padding(Width, Padding) :-
    length(SpaceList, Width),
    maplist(=(' '), SpaceList),
    atomic_list_concat(SpaceList, '', Padding).

clean_hint_list_col(Hints, Cleaned) :-
    exclude(==(0), Hints, NoZeros),
    maplist(number_string, NoZeros, Cleaned).

format_col_hint_line(Hints, Line) :-
    maplist(pad_hint, Hints, Final),
    atomic_list_concat(Final, '', Line).

pad_hint(Hint, Out) :-
    format(string(Out), "~|~w~t~4+", [Hint]).

pad_column_hints([], _, []).
pad_column_hints([H|T], Max, [Padded|Rest]) :-
    length(H, Len),
    Pad is Max - Len,
    length(Suffix, Pad),
    maplist(=(""), Suffix),
    append(Suffix, H, Padded),
    pad_column_hints(T, Max, Rest).

max_column_height(Hints, Max) :-
    maplist(length, Hints, Lengths),
    max_list(Lengths, Max).

get_row_hint_width([], 0).
get_row_hint_width([[Str]|T], MaxWidth) :-
    string_length(Str, Len),
    get_row_hint_width(T, Rest),
    max_list([Len, Rest], MaxWidth).

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

    format("~s║ ➤ 💾 Salvar jogo (Use a tecla v)                   ║~n", [SuccessColor]),
    format("~s", [Reset]),

    format("~s╚════════════════════════════════════════════════════╝~n~n", [TitleColor]),
    format("~s", [Reset]).

show_victory(GameState) :-
    draw_ui(GameState),
    write('\n🎉 Você venceu! Parabéns! 🎉\n').

show_game_over(GameState) :-
    draw_ui(GameState),
    write('\n☠️  Game Over! Tente novamente.\n').

cleanup_systems :-
    format("~sSistema encerrado com segurança.~n", [constants:title_color]),
    constants:reset_color, nl.