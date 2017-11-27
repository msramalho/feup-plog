:- use_module(library(lists)).
:- use_module(library(clpfd)).
clear:-write('\33\[2J').

qm3(L):-
	%1 declarar variáveis
	L = [A, B, C, D, E, F, G, H, I],
	
	%2 declarar domínio
	domain(L, 1, 9),
	
	%3 declarar restrições
	all_distinct(L),
	%LINHAS
	A + B + C #= Sum,
	D + E + F #= Sum,
	G + H + I #= Sum,
	%DIAGONAIS
	A + E + I #= Sum,
	C + E + G #= Sum,
	%COLUNAS
	A + D + G #= Sum,
	B + E + H #= Sum,
	C + F + I #= Sum,
	%evitar soluções espelhadas
	A #< C, %evitar espelho vertical
	A #< G, %evitar espelho horizontal
	B #< D, %evitar espelho diagonal
	
	write(L),
	
	labeling([], L).

qm(N, L).
	
	