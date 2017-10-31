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

getBoardTopColor(X, Y, TopColor):-
    board(B),
    nth0(X, B, Line), %get the line
    nth0(Y, Line, [TopColor|_]). %get the cell and the head which is the top color


%not implementation
not(X):-X, !, fail.
not(_).



clear:-write('\33\[2J').