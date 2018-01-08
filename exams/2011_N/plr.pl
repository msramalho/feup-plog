:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% 6)

threeNumbers(A, B, C):-
	Numbers = [A, B, C],
	% a)
	RPrime = {1}\/{2}\/{3}\/{5}\/{7}\/{9},
	A in RPrime,
	% b)
	B in 0..100,
	% c)
	C in RPrime, 
	C #\= 2,
	% d)
	Tens #= B / 10, 
	B - 10 * Tens #= A,
	% e)
	A #> C,
	% f)
	all_distinct(Numbers),
	% g)
	sum(Numbers, #=, PSquare),
	N in 1..11,
	N * N #= PSquare,
	
	labeling([], Numbers).

% 7
% a) [maximize(Vars), down], não precisa do maximize
% a) [maximize(Vars), down, ffc]

% 8
p8(Pecas):-
	Pecas = [P1, P2, P3, P4, P5, P6],
	domain(Pecas, 0, 24),
	Ends = [E1, E2, E3, E4, E5, E6],
	Tasks = [
		task(P1, 2, E1, 1, 1),
		task(P2, 3, E2, 1, 2),
		task(P3, 5, E3, 1, 3),
		task(P4, 1, E4, 1, 4),
		task(P5, 3, E5, 1, 5),
		task(P6, 6, E6, 1, 6)
	],
	cumulative(Tasks),
	maximum(Last, Ends), Last #=< 25, % domain(Ends, 1, 25)
	P1 #> P6 #=> P1 - E6 #> 5
	#\/
	P1 #< P6 #=> P6 - E1 #> 5,
	P5 #> P1,
	P3 #> P1 #=> P3 #= 4,
	labeling([ffc], Pecas).
	
	

% p9 a
sequencia(Seq, Custo, N):-
	length(Seq, N),
	global_cardinality(Seq, [1-C1, 2-C2, 3-C3, 4-C4]),
	C1 in 5..15, C2 in 2..6, C3 in 5..10, C4 in 7..12,
	automaton(Seq, _, Seq,
		[source(a),source(b),source(c),source(d),sink(a),sink(b),sink(c),sink(d)],
		[
			arc(a,2,b,[C+5]),
			arc(a,3,c,[C+5]),
			arc(a,4,d,[C+7]),
			arc(b,1,a,[C+3]),
			arc(b,4,d,[C+4]),
			arc(c,1,a,[C+2]),
			arc(c,4,d,[C+6]),
			arc(d,1,a,[C+9]),
			arc(d,2,b,[C+5])
		],
		[C],[0],[Acc],[counterseq(Costs)]),
		append([Acc], Seq, Vars),
		labeling([minimize(Acc),ffc], Vars),
		Costs = [_,Sec|_],
		Custo is Acc - Sec.
		
% p9 b
sequenciab(Seq, Custo, N):-
	length(Seq, N),
	global_cardinality(Seq, [1-C1, 2-C2, 3-C3, 4-C4]),
	C1 in 5..15, C2 in 2..6, C3 in 5..10, C4 in 7..12,
	automaton(Seq, _, Seq,
		[source(a),source(b),source(c),source(d),sink(a),sink(b),sink(c),sink(d)],
		[
			arc(a,2,b,[C+5]),
			arc(a,3,c,[C+5]),
			arc(a,4,d,[C+7]),
			arc(b,1,a,[C+3]),
			arc(b,4,d,[C+4]),
			arc(c,1,a,[C+2]),
			arc(c,4,d,[C+6]),
			arc(d,1,a,[C+9]),
			arc(d,2,b,[C+5])
		],
		[C],[0],[Acc],[counterseq(Costs)]),
		
		% parte b)
		append([_Left, [V1,V2,V3], Right], Seq),
		Right = [FirstR, RestR],
		cumlist(countPairs(4,2), RestR, Matches, FirstR, _),
		count(1, Matches,#=, Count42),
		V1 #= 3 #/\ V2 #= 4 #/\ V3 #= 2 #=> Count42 #=< 1,		
		
		append([Acc], Seq, Vars),
		labeling([minimize(Acc),ffc], Vars),
		Costs = [_,Sec|_],
		Custo is Acc - Sec.
	
countPairs(A, B, Current, Match, Previous, Current):-
	A #= Previous #/\B #= Current #<=> Match.
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

