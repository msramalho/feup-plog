:- use_module(library(system)).
:- use_module(library(random)).
:- use_module(library(lists)).


setRandomSeed:-
	now(Time), S is Time mod 30269,
	getrand(random(X, Y, Z, _)),
	setrand(random(S, X, Y, Z)), !.
:-setRandomSeed. %set a random seed

listToString(List, Result):-
	listToString(List, '', Result).

listToString([], Final, Final).
listToString([H|T], Temp, Final):-
	atom_concat(Temp, H, NewRes),
	listToString(T, NewRes, Final).

numberToString(Number, String):-
	number_chars(Number, TempList),
	listToString(TempList, String).

writeString(_, 0).
writeString(String, Count):-
    write(String),
    NewCount is Count - 1,
    writeString(String, NewCount).

%https://stackoverflow.com/questions/41454755/how-can-i-replace-an-element-of-a-list-using-an-index-in-prolog
replaceAt([_|T],0,E,[E|T]).
replaceAt([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, replaceAt(T,NP,E,R).

%replace a stack at the Line, Column by the NewStack
replaceBoardStack(Board, X, Y, NewStack, NewBoard):-
    nth0(X, Board, Line),
    replaceAt(Line, Y, NewStack, NewLine), %replace the cell for the new piece
    replaceAt(Board, X, NewLine, NewBoard).


getBoardTopColor(X, Y, TopColor):-
    board(B),
    nth0(X, B, Line), %get the line
    nth0(Y, Line, [TopColor|_]). %get the cell and the head which is the top color

getBoardStack(X, Y, Stack):-
    board(B),
    nth0(X, B, Line), %get the line
    nth0(Y, Line, Stack). %get the Stack

%get the stack height (getBoardStackHeight/4 sets the last as the Stack)
getBoardStackHeight(X, Y, Height):-getBoardStackHeight(X, Y, Height, _).
getBoardStackHeight(X, Y, Height, Stack):-
    getBoardStack(X, Y, Stack),
    length(Stack, Height).

%not implementation
not(X):-X, !, fail.
not(_).



clear:-write('\33\[2J').