:-use_module(library(clpfd)).
% :-use_module(library(lists)).
clear:-write('\33\[2J').

sequencia(Seq, Custo, N):-
	length(Seq, N),
	domain(Seq, 1, 4),
	automaton(
		Seq, _, Seq,
		[source(a), source(b), source(c), source(d), sink(a), sink(b), sink(c), sink(d)],
		[
			arc(a,2,b, [C+5,I+1]),
			arc(a,3,c, [C+5,I+1]),
			arc(a,4,d, [C+7,I+1]),
			arc(b,1,a, [C+3,I+1]),
			arc(b,4,d, [C+4,I+1]),
			arc(c,1,a, [C+2,I+1]),
			arc(c,4,d, [C+6,I+1]),
			arc(d,1,a, [C+9,I+1]),
			arc(d,2,b, [C+5,I+1])
		],
		[C, I], [0, 0], [Custo, N], []
	),

	count(1, Seq, #=, Count1), Count1 in 5..15,
	count(2, Seq, #=, Count2), Count2 in 2..6,
	count(3, Seq, #=, Count3), Count3 in 5..10,
	count(4, Seq, #=, Count4), Count4 in 7..12,

	Vars = [Custo|Seq],
	labeling([minimize(Custo)], Vars).