:-use_module(library(clpfd)).
:-use_module(library(lists)).
% :-use_module(library(statistics)).
:-use_module(library(between)).
% :-use_module(library(sets)).

clear:-write('\33\[2J').


table6(People):-
	People = [A, B, C, D, E, F],
	
	domain(People, 1, 6),
	all_distinct(People),
	
	Lines = [
		person(A, 1),
		person(B, 1),
		person(C, 1),
		person(D, 1),
		person(E, 1),
		person(F, 1)
	],
	
	together(A, B),
	together(C, D),
	notTogether(E, F),
	notTogether(A, E),
	
	% minimum(A, People),
	A #= 1,
	
	disjoint1(Lines, [wrap(1,7)]),
	
	labeling([], People).

together(A, B):- A #= B + 1 #\/ B #= A + 1.
notTogether(A, B):- A #\= B + 1 #/\ B #\= A + 1.

% b
% L = [A, B, C, D, E, F, G, H], squareTable(L, 8, [A-B, C-D, E-F, G-H], [A-C, B-C, A-E, D-G, D-H, E-G, E-H]).
squareTable(People, Size, Together, NotTogether):-
	domain(People, 1, Size),
	all_distinct(People),
	
	getDisjoinLines(People, Lines),
	SizeWrap is Size + 1,
	disjoint1(Lines, [wrap(1,SizeWrap)]),
	
	HalfSize is integer(Size/2),
	numlist(2, HalfSize, List),
	findall(X-Y, (member(X, List), Y is Size - X + 2),FixedNotTogether),
	
	append(FixedNotTogether, NotTogether, AllNotTogether),

	setNotTogether(AllNotTogether),
	setTogether(Together),

	People = [First|_], First #= 1,
	
	labeling([], People).

getDisjoinLines([], []).
getDisjoinLines([P|People], [person(P, 1)|Lines]):- getDisjoinLines(People, Lines).

setNotTogether([]).
setNotTogether([X-Y|T]):- setNotTogether(T), notTogether(X,Y).

setTogether([]).
setTogether([X-Y|T]):- setTogether(T), together(X,Y).
