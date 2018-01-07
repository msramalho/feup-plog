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
	length(Visitas, NDias),
	
	%domains
	findall([I,X,Y], concelho(I, X, Y), TextConcelhos),
	findall(Dist, nth1(_, TextConcelhos, [_, Dist, _NEle]), LDist),
	findall(NEle, nth1(_, TextConcelhos, [_, _Dist, NEle]), LNEle),
	write(TextConcelhos),
	nl, write(LDist),
	nl, write(LNEle),
	length(TextConcelhos, NConcelhos),
	domain(Visitas, 0, NConcelhos), % 0 ate aos possiveis
	
	%constraints
	getTotal(Visitas, LDist, DistTotal),
	DistTotal #=< MaxDist,
	getTotal(Visitas, LNEle, TotalEleitores),
	
	nl, write('labeling'),
	nl, write(Visitas),
	%labeling
	% labeling([maximize(TotalEleitores)], Visitas),
	indomain(DistTotal), nl, write(DistTotal).
	% translateCvs(TextConcelhos, Visitas, ConcelhosVisitados).
	
getTotal([], _List, 0).
getTotal([0|T], List, Total):- !, getTotal(T, List, Total).%ignores 0
getTotal([C|T], List, Total):-
	getTotal(T, List, AccTotal),
	element(C, List, Val),
	Total #= AccTotal + Val.

translateCvs(_Concelhos, [0], []):-!.
translateCvs(Concelhos, [C], [Val]):- nth1(1, C, Index), nth1(Index, Concelhos, concelho(Val, _, _)).
translateCvs(Concelhos, [0|T], Res):-!,translateCvs(Concelhos, T, Res).
translateCvs(Concelhos, [C|T], Res):-
	translateCvs(Concelhos, T, NewRes),
	nth1(C, Concelhos, Conc), 
	nth1(1, Conc, Val),
	append([Val], NewRes, Res).
	
	
	
	
	
	
	
	
	
	
	
	
	
	