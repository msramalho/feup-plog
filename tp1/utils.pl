:- use_module(library(system)).
:- use_module(library(random)).

setRandomSeed:-
	now(Time), S is Time mod 30269,
	getrand(random(X, Y, Z, _)),
	setrand(random(S, X, Y, Z)), !.

listToString(List, Result):-
	listToString(List, '', Result).
listToString([], Final, Final).
listToString([H|T], Temp, Final):-
	atom_concat(Temp, H, NewRes),
	listToString(T, NewRes, Final).

numberToString(Number, String):-
	number_chars(Number, TempList),
	listToString(TempList, String).