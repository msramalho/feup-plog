

/*
evaluate.pl: This file implements the predicates required for generating a random board
*/

/*The board is a matrix of 13 lines and 9 columns, not all are used */
emptyBoard([
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []],
    [[], [], [], [], [], [], [], [], []]
]).


% a list of all the pieces that enter the board at the beginning 8 of each and 3 of wild
piecesToDistribute([
    black, black, black, black, black, black, black, black,
    red, red, red, red, red, red, red, red,
    ivory, ivory, ivory, ivory, ivory, ivory, ivory, ivory,
    green, green, green, green, green, green, green, green,
    blue, blue, blue, blue, blue, blue, blue, blue,
    wild, wild, wild
]).


fillBoard(Board, [], [], Board).
fillBoard(Board, [Position|OtherPositions], Pieces, FinalBoard):-
    X-Y = Position,
    %format('(~d,~d)', [X, Y]),
    random_select(Piece, Pieces, NewPieces),
    NewCell = [Piece],
    replaceBoardStack(Board, X, Y, NewCell, NewBoard), %replace the cell for the new piece
    fillBoard(NewBoard, OtherPositions, NewPieces, FinalBoard).


generateBoard(NewBoard):-
    getValidPositions(Positions),
    piecesToDistribute(Pieces),
    emptyBoard(E),
    fillBoard(E, Positions, Pieces, NewBoard).
