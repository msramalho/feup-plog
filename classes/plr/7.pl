:- use_module(library(lists)).
:- use_module(library(clpfd)).
clear:-write('\33\[2J').


perus(L, M72U):-
	%72 perus
	%_67_
	%M67U
	%PPP preço por peru
	
	%1 declarar variáveis
	L = [M, U, PPP],
	
	%2 declarar domínio
	M in 1..9, % m nao pode ser 0
	U in 0..9, % podiamos retringir a numeros pares porque Preco/72 tem de ser inteiro e N/par ser inteiro tem de ser par
	
	%3 declarar restrições
	PPP in 23..135, %otimizar as restrições
	PPP*72 #= M * 1000 + 6 * 100 + 7 * 10 + U,
	%P in 1670..9999 % alternative
	
	%4 pesquisar solucao
	labeling([], L).
	
	