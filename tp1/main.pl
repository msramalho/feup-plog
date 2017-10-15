:- include('evaluate.pl').
:- include('display.pl').
:- include('board.pl').
:- include('examples.pl').

test:-
    emptyBoard(B),
    P1 = [red, green],
    P2 = [ivory, empty],
    Playing = player1,
    displayBoard(B, P1, P2, Playing).