:-use_module(library(clpfd)).
clear:-write('\33\[2J').

%5
% 1 - amarelo, 2 - azul, 3 - verde, 4 - vermelho
seq(N, CoresAtoms, Custo):-
	% variables
	length(Cores, N),
	
	% domain
	domain(Cores, 1, 4),
	Cores = [First|_],
	First = 1, %first is yellow, avoid symmetries
	
	CountYellow in 2..N,
	CountBlue in 1..N,
	CountRed in 1..N,
	CountGreen #= 3,
	
	% constraints
	% a)
	count(1, Cores, #=, CountYellow),
	count(2, Cores, #=, CountBlue),
	CountYellow #> CountBlue,
	% b)
	count(3, Cores, #=, CountGreen), 
	% c) - all the yellows come in the beginning
	length(Yellow, CountYellow),
	append(Yellow, Rest, Cores),
	% d)
	Must = [2,3,4],
	append(_, Must, Part1),
	append(Part1, _, Rest), % could be Cores, but no need
		
	% labeling
	count(4, Cores, #=, CountRed),
	Custo #= 3 * CountYellow + 1 * CountBlue + 5 * CountGreen + 2 * CountRed,
	labeling([minimize(Custo),ffc], Cores),
	translate(Cores, CoresAtoms).
	
translate([], []).
translate([1|Cores], [amarelo|Translated]):-translate(Cores, Translated).
translate([2|Cores], [azul|Translated]):-translate(Cores, Translated).
translate([3|Cores], [verde|Translated]):-translate(Cores, Translated).
translate([4|Cores], [vermelho|Translated]):-translate(Cores, Translated).









