:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% concelho(Nome,Distancia,NEleitoresIndecisos)
concelho(x,120,410).
concelho(y,10,800).
concelho(z,543,2387).
concelho(w,3,38).
concelho(k,234,376).


% concelhos(3,700,Visitas,Dist,TE).
% concelhos(NDias, MaxDist, ConcelhosVisitados, DistTotal, TotalEleitores):-
concelhos(NDias, MaxDist, ConcelhosVisitados, DistTotal, TotalEleitores):-
	%variables
	findall([Nome, Distancia, NEleitoresIndecisos], concelho(Nome,Distancia,NEleitoresIndecisos), Concelhos),
	maplist(getIndex(2),Concelhos,Dists),
	maplist(getIndex(3),Concelhos,NEles),
	same_length(Concelhos, Visitas),
	
	%domains
	domain(Visitas,0,1),
	
	%constraints
	count(1, Visitas, #=<, NDias),
	scalar_product(Dists, Visitas, #=, DistTotal), 
	fd_dom(DistTotal, DT), write(DT), nl, 
	DistTotal #=< MaxDist,
	scalar_product(NEles, Visitas, #=, TotalEleitores), 
	
	%labeling
	labeling([maximize(TotalEleitores)],Visitas),
	findall(C, (nth1(I, Visitas, 1), nth1(I, Concelhos, [C,_,_])), ConcelhosVisitados).
	
	
getIndex(Index, Concelho, Res):- nth1(Index, Concelho, Res).
	
	
	
	
	
	
	
	
	