:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% 4 a)
square(Vars):-
	length(Vars, 9),
	domain(Vars, 1, 9),
	all_distinct(Vars),
	length(Parts, 3),
	maplist(same_length(Parts), Parts),
	maplist(sumTo(15),Parts),
	append(Parts, Vars),
	labeling([], Vars).
	
sumTo(Val, Part):-sum(Part, #=, Val).

% 4 b)
% squares(Vars, N):-
	% Len #= N * 9,
	% length(Vars, Len),
	
	% domain(Vars, 1, 9),
	
	% length(Blocks, N),
	% append(Blocks, Vars),
	% maplist(all_distinct, Blocks),
	
	% NParts #= N * 3,
	% length(Parts, NParts),
	% maplist(sumToAndLength(15),Parts),
	% Parts = [First|Rest],
	% cumlist(diff_adjacent, Rest, First, _),
	% append(Parts, Vars),
	
	% labeling([down], Vars).
	
% sumToAndLength(Val, Part):-
	% length(Part, 3),
	% Part = [A,B,C],
	% A #> B #/\ B #> C,
	% sum(Part, #=, Val).
	

% diff_adjacent(Line, Prev, Line):-
	% head(Line, Lh),
	% head(Prev, Lp),
	% Lh #\= Lp.
	
% 4 c)
% Dado que se pretende ordem decrescente por linha e que há uma restrição extra entre os algarismos iniciais de cada linha, a eficiência de execução pode ser melhorada fazendo com que os valores iniciais sejam selecionados primeiro, ou seja, usando, por exemplo, a flag ffc que atinge primeiro as variáveis com mais restrições e, de seguida, a flag down, que, ao começar por atribuir maiores valores de domínio aos primeiros elementos das linhas (podem não ser todos logo selecionados pelo ffc, mas pelo menos 1 será) então temos que a restrição de ordem decrescente é mais rapidamente respeitada do que se se comessassem por valores pequenos de domínio, para tal recomendo o uso da flag down.

% 4 d)

squares(Vars, N):-
	Len #= N * 9,
	length(Vars, Len),
	
	domain(Vars, 1, 9),
	
	length(Blocks, N),
	append(Blocks, Vars),
	maplist(all_distinct, Blocks),
	
	NParts #= N * 3,
	length(Parts, NParts),
	maplist(sumToAndLength(15),Parts),
	Parts = [First|Rest],
	cumlist(diff_adjacent, Rest, First, _),
	%mudou daqui
	maplist(convertMult4, Parts, Mult4),
	Counter in 1..3,
	count(2, Mult4, #=, Counter),
	%mudou até aqui
	
	append(Parts, Vars),
	
	labeling([down], Vars),
	write(Mult4).
	
sumToAndLength(Val, Part):-
	length(Part, 3),
	Part = [A,B,C],
	A #> B #/\ B #> C,
	sum(Part, #=, Val).
	

diff_adjacent(Line, Prev, Line):-
	head(Line, Lh),
	head(Prev, Lp),
	Lh #\= Lp.


convertMult4([X, Y, Z], Counter):-
	X mod 4 #=0 #<=> B1,
	Y mod 4 #=0 #<=> B2,
	Z mod 4 #=0 #<=> B3,
	Counter #= B1 + B2 + B3.

	
	
% 5
seq(Sequence, N, CostFinal):-
	% variables
	TotalPieces #= N * 4,
	length(Sequence, TotalPieces),
	
	% domain
	domain(Sequence, 0, 3),
	
	% constraints
	global_cardinality(Sequence, [0-N,1-N,2-N,3-N]),
	% count(0, Sequence, #=, N),
	% count(1, Sequence, #=, N),
	% count(2, Sequence, #=, N),
	% count(3, Sequence, #=, N),
	
	automaton(
		Sequence, _, Sequence,
		[source(a), source(b), source(d), sink(a), sink(b), sink(c), sink(d)],
		[			
			arc(a,1,b,[C+3]),
			arc(a,2,c,[C+3]),
			arc(a,3,d,[C+2]),
			arc(b,3,d,[C+1]),
			arc(c,0,a,[C+1]),
			arc(c,1,b,[C+4]),
			arc(d,1,b,[C+5]),
			arc(d,3,d,[C+2])
		],
		[C], [0], [Cost],
		[counterseq(Counters)]
	),
	
	% labeling
	labeling([minimize(Cost)],Sequence),
	%correct cost value, which includes the first transition cost, wrongly
	Counters = [_,Second|_],
	CostFinal is Cost - Second.
	
	
	
	
	
	
	
	
	
	
	
	