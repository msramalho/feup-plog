:-use_module(library(clpfd)).
%use_module(library(lists)).
clear:-write('\33\[2J').

% constroi(30, 100, [6, 4, 12, 20, 6], [20, 50, 10, 20, 15], E, O).
constroi(NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, Objetos):-
	%1 variaveis
	length(Objetos, 4), %cada elemento tem o index do objeto (correspondendo ao de EmbPorObjeto e CustoPorObjeto)
	
	%2 dominio
	length(EmbPorObjeto, N), % quantos objetos diferentes
	domain(Objetos, 1, N), %Objeto em Objetos pertence aos possíveis (1 até N)
	
	%3 restrições
	all_distinct(Objetos), % 4 diferentes
	sumById(Objetos, EmbPorObjeto, EmbUsadas),
	sumById(Objetos, CustoPorObjeto, CustoTotal),
	EmbUsadas #=< NEmb,
	CustoTotal #=< Orcamento,
	
	%4 labeling
	labeling([maximize(EmbUsadas)], Objetos).
	
% sumById(Ids, ToSum, Sum).
sumById([], _, 0).
sumById([Id|Ids], ToSum, Sum):-
	sumById(Ids, ToSum, AccSum),
	element(Id, ToSum, CurrSum),
	Sum #= AccSum + CurrSum.











