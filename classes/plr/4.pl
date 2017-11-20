:- use_module(library(lists)).
:- use_module(library(clpfd)).
clear:-write('\33\[2J').


sumWord([], [], []).
sumWord([E1|T1], [E2|T2], [E3|L3]):-
	E1 + E2 #= E3,
	sumWord(T1, T2, T3).
	
puzzle(Sum, L1, L2, L3):-
	%simplificar para as restrições
	append([L1, L2, L3], DuplicateLetters),
	remove_dups(DuplicateLetters, L), 
	
	%declarar variáveis já está, são as que chegam
	%declarar domínios
	sumWord(L1, L2, L3), !, 
	
	domain(L, 0, 9),
	all_distinct(L),
	
	
	labeling([], L).
	
%bagof(X, Y^pred(X, Y), Results).