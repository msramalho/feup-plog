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

/* Available pieces to place in the board*/
availablePiecesToPlace([
    ['D', 8],
    ['R', 8],
    ['I', 8],
    ['G', 8],
    ['B', 8],
    ['W', 3]
]).

%https://stackoverflow.com/questions/41454755/how-can-i-replace-an-element-of-a-list-using-an-index-in-prolog
replaceAt([_|T],0,E,[E|T]).
replaceAt([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, replaceAt(T,NP,E,R).


piecesToDistribute([['D'], ['R'], ['I'], ['D']]).

nextPosition([], 0, 0, []):-write('end of next position'),nl.
nextPosition(Positions, Index, X, Y):-
    nth0(0, Positions, CoordinatesX),
    nth0(1, Positions, CoordinatesY),
    nth0(Index, CoordinatesX, X),
    nth0(Index, CoordinatesY, Y),
    write('X: '), write(X), nl,
    write('Y: '), write(Y), nl.

fillBoard(Board, _, _, [], Board).
fillBoard(Board, Positions, Index, Pieces, FinalBoard):-
    nextPosition(Positions, Index, X, Y),           %get the x and y for the next valid position at index
    length(Pieces, Len),                            %get the number of pieces left to distribute
    random(0, Len, PieceIndex),                     %choose a random piece from the lists
    nth0(PieceIndex, Pieces, PieceChar, RemovedPieces),%chose and remove piece from pieces
    NewBoard = Board,
    nth0(X, NewBoard, LineToChange),                %get the Xth line of the board
    replaceAt(LineToChange, Y, PieceChar, NewLine), %replace the cell for the new piece
    replaceAt(NewBoard, X, NewLine, TempFinalBoard),    %replace the line in the board for the new one (check id necessary, because nht0 may return a reference and not a copy)
    NewIndex is Index + 1,
    FinalBoard = TempFinalBoard,
    write('Index is done: '), write(Index), nl,
    fillBoard(TempFinalBoard, Positions, NewIndex, RemovedPieces, FinalBoard).



getRandomBoard(NewBoard):-
    getValidPositions(Positions),
    piecesToDistribute(Pieces),
    emptyBoard(E),
    fillBoard(E, Positions, 0, Pieces, NewBoard).
    /*,

    nextPosition(Positions, X, Y, NewPositions),
    Positions = NewPositions,
    length(Pieces, L),
    random(0, L, Novo),
    translate(Novo, Val),
    write('Novo is: '),
    write(Val),
    P = Pieces.*/

translate(0, 'D'). %Black
translate(1, 'R'). %Red
translate(2, 'I'). %Ivory
translate(3, 'G'). %Green
translate(4, 'B'). %Blue
translate(5, 'W'). %WildColor