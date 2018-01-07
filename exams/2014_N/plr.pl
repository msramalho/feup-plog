:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

%4 a) - opção 2 - única que não provoca backtracking fora do labeling

%4 b)
sequence(L):-
	length(L, 6),
	
	domain(L, 1, 7),
	
	all_distinct(L),
	constrainPairs(L),
	
	%avoid symmetry
	L = [First|_], last(L, Last),
	First #< Last,
	
	labeling([], L).
	
constrainPairs([_]).
constrainPairs([A,B|T]):-
	constrainPairs([B|T]),
	Odd1 #= A mod 2, Odd2 #= B mod 2,
	(Odd1 #= 0 #/\ Odd2 #\= 0) #\/ (Odd1 #\= 0 #/\ Odd2 #= 0),
	A #\= B + 1 #/\ A #\= B - 1.
	

%4 c)
sequence(N, L):-
	length(L, N),
	
	Max is N + 1,
	domain(L, 1, Max),
	
	all_distinct(L),
	
	%avoid symmetry
	L = [First|_], last(L, Last),
	First #< Last,
	
	constrainPairs(L),
	constrainPairs([First,Last]),
	
	labeling([], L).