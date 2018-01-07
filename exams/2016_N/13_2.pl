:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% concelho(Nome,Distancia,NEleitoresIndecisos)
concelho(x,120,410).
concelho(y,10,800).
concelho(z,543,2387).
concelho(w,3,38).
concelho(k,234,376).


% concelhos(3,700,CVs,Dist,TE).
% concelhos(NDias, MaxDist, ConcelhosVisitados, DistTotal, TotalEleitores):-
concelhos(NDias, MaxDist, ConcelhosVisitados, DistTotal, TotalEleitores):-
	%variables
	findall([Nome, Dist, Elem], concelho(Nome, Dist, Elem), Concelhos),
	findall(Nome, nth1(_, Concelhos, [Nome, _, _]), LNome),
	findall(Elem, nth1(_, Concelhos, [_, _, Elem]), LElem),
	findall(Dist, nth1(_, Concelhos, [_, Dist, _]), LDist),
	length(Concelhos, NConcelhos),
	length(Visitas, NConcelhos),
	%domains
	domain(Visitas, 0, 1),
	
	%constraints
	count(1, Visitas, #=<, NDias),
	scalar_product(LDist, Visitas, #=, DistTotal),
	DistTotal #=< MaxDist,
	scalar_product(LElem, Visitas, #=, TotalEleitores),
	
	%labeling
	labeling([maximize(TotalEleitores)], Visitas),
	translateVisitas(LNome, Visitas, ConcelhosVisitados, 0).
	
	
translateVisitas(_LNome, [], [], _).
translateVisitas(LNome, [0|V], Res, Counter):- 
	NewCounter is Counter + 1,
	translateVisitas(LNome, V, Res, NewCounter).
translateVisitas(LNome, [1|V], Res, Counter):-
	NewCounter is Counter + 1,
	translateVisitas(LNome, V, NewRes, NewCounter),
	nth0(Counter, LNome, Curr),
	append([Curr], NewRes, Res).













	
	
	
	