:- use_module(library(random)).
:- use_module(library(lists)).

/*The board is a matrix of 13 lines and 9 columns, not all are used */
emptyBoard([
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]]
]).

%https://stackoverflow.com/questions/41454755/how-can-i-replace-an-element-of-a-list-using-an-index-in-prolog
replaceAt([_|T],0,E,[E|T]).
replaceAt([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, replaceAt(T,NP,E,R).


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
    nth0(X, Board, Line),
    NewCell = [Piece],
    replaceAt(Line, Y, NewCell, NewLine), %replace the cell for the new piece
    replaceAt(Board, X, NewLine, NewBoard),
    fillBoard(NewBoard, OtherPositions, NewPieces, FinalBoard).


getRandomBoard(NewBoard):-
    getValidPositions(Positions),
    piecesToDistribute(Pieces),
    emptyBoard(E),
    fillBoard(E, Positions, Pieces, NewBoard).
