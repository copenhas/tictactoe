-module(ttt_board).

-export([new/0, place/3, victory/1]).


new() ->
    {{n, n, n},
     {n, n, n},
     {n, n, n}}.

place(Board, Piece, {X, Y}) when Piece==x; Piece==o ->
    Row = element(Y, Board),
    case element(X, Row) of
        n -> 
            NewRow = setelement(X, Row, Piece),
            setelement(Y, Board, NewRow);
        _ -> {error, invalid_move}
    end.

victory(Board) ->
    Rows = rows(Board),
    Straights = columns(Board) ++ Rows,
    AllOptions = diagonals(Board) ++ Straights,

    case [P || {P, P, P} <- AllOptions, P==x orelse P==o] of
        [] -> none;
        [Winner] -> {winner, Winner}
    end.

rows(Board) ->
    [element(1, Board), element(2, Board), element(3, Board)].

columns(Board) ->
    Left = {element(1, element(1, Board)),
            element(1, element(2, Board)),
            element(1, element(3, Board))},
    Middle = {element(2, element(1, Board)),
              element(2, element(2, Board)),
              element(2, element(3, Board))},
    Right = {element(3, element(1, Board)),
             element(3, element(2, Board)),
             element(3, element(3, Board))},
    [Left, Middle, Right].

diagonals(Board) ->
    Back = {element(1, element(1, Board)),
            element(2, element(2, Board)),
            element(3, element(3, Board))},
    Forward = {element(3, element(1, Board)),
               element(2, element(2, Board)),
               element(1, element(3, Board))},
    [Back, Forward].

