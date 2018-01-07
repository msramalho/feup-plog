:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(between)).
clear:-write('\33\[2J').

% 4 a)
seq(Vars):-
	length(Vars, 5),
	
	domain(Vars, 1, 9),
	
	% constraint 1
	contraintPairs(Vars),
	
	% constraint 2
	same_length(Left, Right, 2),
	Center in 1..2,
	append(Left, [Center], CLeft),
	append(CLeft, Right, Vars),
	
	% constraint 3
	all_distinct(Vars),
		
	labeling([ffc],Vars).
	
contraintPairs([_]).
contraintPairs([X,Y|T]):-
	contraintPairs([Y|T]),
	((X mod 2 #= 0 #/\ Y mod 2 #\= 0) 
	#\/
	(X mod 2 #\= 0 #/\ Y mod 2 #= 0)).
	
% 4 b)
seq(Vars, N):-
	N mod 3 #= 0,
	N in 3..30,
	
	length(Vars, N),
	
	domain(Vars, 1, 9),
	
	numlist(1, 9, Ascending),
	maplist(maxCount(Vars, 3), Ascending),
	
	contraintPairs(Vars),
	
	% last constraint
	head(Vars, First),
	append(_, [Last], Vars),
	Center #> First #/\ Center #> Last,	
	
	Half #= (N-1) / 2,
	same_length(Left, Right, Half),
	append(Left, [Center], CLeft),
	append(CLeft, Right, Vars),
	
	labeling([], Vars).

maxCount(Vars, N, V):-
	count(V, Vars, #=<, N).

% 4 c)
% ffc porque o Center é o que tem mais restrições, seguido de um down, para que sejam atribuídos números altos primeiro, ao centro, o que rapidamente se propaga para o inicial e final e consequentemente para os seus adjacentes, e seus adjacentes, restringindo muito o domínio permitido logo à partida. Destas duas a mais crítica seria ffc, dado que sem ela a operação de etiquetagem não começa pelo valor central e se desperdiça as características das restrições aplicadas

% 5
board_values([3,5,7,7,2,6,2,7,2,4,6,3,9,4,8,6,2,1,8,9,5,9,3,4,5,1,4,8,8,4,7,8,4,3,8,9,7,8,8,4,1,7,9,8,3,3,8,5,3,2,4,5,2,6,1,2,2,7,1,1,8,1,7,2]).

horse(X, Y, TotalValue, N):-
	board_values(BV),
	% variables
	same_length(X, Y, N),
	
	% domain
	domain(X, 1, 8), domain(Y, 1, 8),
	% getXY(X, Y, XYs), % alternative with cumlist
	cumlist(mergeXY, X, Y, 0, XYs),
	
	% constraints
	XYs = [First|Rest],
	cumlist(validJump, Rest, First, _),
	
	% labeling
	maplist(values(BV), XYs, Values),
	sum(Values, #=, TotalValue),
	append(X, Y, Vars),
	labeling([maximize(TotalValue)], Vars).

mergeXY(X, Y, _, X-Y).

values(BV, X-Y, Value):-
	Index #= X + (Y - 1) * 8,
	element(Index, BV, Value).

validJump(X-Y, Xb-Yb, X-Y):-
	Xb + Jx #= X #/\ Yb + Jy #= Y,
	domain([Jx, Jy],-2,2),
	Jx #\= 0 #/\ Jy #\=0,
	% domain([X,Y], 1, 8), % redundant
	abs(Jx) #\= abs(Jy).
	
% getXY([], [], []).
% getXY([X|Tx], [Y|Ty], [X-Y|Rest]):-getXY(Tx,Ty,Rest).
