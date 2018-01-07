:-use_module(library(clpfd)).
% :-use_module(library(lists)).
clear:-write('\33\[2J').
%12 min
ups_and_downs(Min, Max, N, L):-
	%variables
	length(L, N),
	
	%domain
	domain(L, Min, Max),
	
	%constraints
	constraintAdjacent(L),
	
	%labeling
	labeling([ffc], L).
	
constraintAdjacent([_]).
constraintAdjacent([SLast, Last]):- SLast #> Last #\/ SLast #< Last.
constraintAdjacent([X1, X2, X3|T]):-
	constraintAdjacent([X2, X3|T]),
	((X2 #> X1 #/\ X2 #> X3)
	#\/
	(X2 #< X1 #/\ X2 #< X3)).
	
% L = [_X1,_X2,_,_], _X1#<_X2, all_different(L), ups_and_downs(1,4,4,L).
% length(L,5), circuit(L), ups_and_downs(1,5,5,L).