:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% a)
/*
sudoku(N, Sudoku):-
	length(Sudoku, N),
	maplist(same_length(Sudoku), Sudoku),
	constrainSudoku(Sudoku, N),
	labeling([], Sudoku).
	
constrainSudoku(_, 0).
constrainSudoku(Sudoku, N):-
	NewN is N - 1,
	constrainSudoku(Sudoku, NewN),
	
	getLine(N, Sudoku, Line),
	length(Line, N),
	% domain(Line, 1, N), % using maplist instead
	all_distinct(Line),
	
	getColumn(N, Sudoku, Column),
	domain(Column, 1, N),
	all_distinct(Column),
	
	getBlock(N, Sudoku, Block),
	domain(Block, 1, N),
	all_distinct(Block).
*/
	
	

% b)
/*
sudoku(N, Sudoku):-
	length(Sudoku, N),
	constrainSudoku(Sudoku, N, K),
	
	K is sqrt(N),
	getDiagonal1(N, Sudoku, D1),
	getDiagonal2(N, Sudoku, D2),
	count(1, D1, #=, K),
	count(1, D2, #=, K),
		
	labeling([], Sudoku).
*/
% c)
% Tendo em conta que uma restrição neste problema é a de atribuir K 1s às diagonais principais, e que essa é a única restrição que manifesta uma heterogeneidade de domíno, o uso da flag up (que já é a escolh por omissão) terá impacto na descoberta de soluções, dado que ao percorrer o domínio por ordem ascendente, a atribuição dos 1s acontece mais cedo na árvore de pesquisa de soluções o que rapidamente restringe um grande número de domínios, face ao caso contrário, down, em que não se tiraria proveito da existência de esta tendência inerente à cláusula dos 1s.
% On second thought, min is the way to go. But up can be left as default.


% 5

% regioes(9, [1-2, 1-3, 2-3, 2-4, 2-5, 3-4, 3-6, 4-5, 4-6, 4-7, 4-8, 5-8, 5-9, 6-7, 7-8, 8-9], Cores). 
regioes(N, Adjacentes, Cores):-
	length(Cores, N),
	% Min is N - 4, % only 4 colors required, at max, there is a theorem on that -> for 3D!!!!!
	domain(Cores, 1, N), 
	% constraint 1
	maplist(diffColors(Cores), Adjacentes),
	
	% constraint 2
	% constraint 2
	% constraint 2
	Sequence = [S1,S2,S3],
	sequence(S1,Cores), sequence(S2,Cores), sequence(S3,Cores), 
	all_distinct(Sequence),
	sequence(S4,Cores),
	S4 #= S1 #\/ S4 #= S2 #\/ S4 #= S3,
	
	% constraint 3
	cumlist(custo(N), Cores, 0, Custos),
	sum(Custos, #=, Custo),
	
	append(Sequence, Cores, Vars),
	labeling([minimize(Custo),down],Vars),
	nl, write(Custos), nl, write(Custo), nl,
	write(Sequence), nl.

diffColors(Cores, A-B):-
	element(A, Cores, CorA),
	element(B, Cores, CorB),
	CorA #\= CorB.
	
custo(N, X, V1, V):-
	write(X-V1),
	V #= N - X + 1.

sequence(First, Cores):-
	element(First, Cores, A),
	Second #= First + 1,
	element(Second, Cores, B),
	Third #= Second + 1,
	element(Third, Cores, C),
	(
		A mod 2 #= 0 #/\
		B mod 2 #\= 0 #/\
		C mod 2 #= 0
	).
	











