:- include('evaluate.pl').
:- include('display.pl').
:- include('board.pl').
:- include('examples.pl').
:- include('interface.pl').

testBoard([
    [[empty], [empty], [empty], [empty], [empty], [empty], [red], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [black, ivory, blue], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [blue, black, red, green], [empty], [empty]],
    [[empty], [empty], [empty], [ivory], [empty], [red], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [green, red, blue, ivory], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [red], [empty], [ivory], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [blue], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [green], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]]
]).

test:-
    testBoard(B),
    P1 = [red, green],
    P2 = [ivory, empty],
    Playing = player1,
    displayBoard(B, P1, P2, Playing).