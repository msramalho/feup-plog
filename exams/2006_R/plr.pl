:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% 4.1)
quadPerf(A, B, C):-
	L = [A, B, C],
	domain(L, 1, 1000000),
	SR in 1..1000,
	A #=< C, 
	(A * B + C #= (SR * SR)),
	
	labeling([],[SR|L]).
	
% 4.2)
clubes(Ordem, Datas):-
	Ordem = [O1,O2,O3,O4,O5],
	Datas = [Porto,Braga,Sporting,Benfica,Belenenses],
	
	domain(Ordem, 1, 5),
	Ds = [1893,1904,1906,1919,1921],
	list_to_fdset(Ds, SDs),
	maplist(inSet(SDs), Datas),
	
	all_distinct(Ordem),
	all_distinct(Datas),
	
	Benfica + 2 #= Sporting,
	element(O3, Datas, 1893),
	
	element(AntesBraga, Datas, 1906),
	append([_, [AntesBraga], [2], _], Ordem),
	
	maximum(UltimoFundado, Ds), UltimoFundado #\= Porto,
	append([_, [1], _, [4], _], Ordem),
	
	element(AntesBenfica, Datas, 1919),
	append([_, [AntesBenfica], [4], _], Ordem),
	
	append(Ordem, Datas, Vars),
	labeling([], Vars).
	
	
inSet(SDs, Data):- Data in_set SDs.


% 4.3)
peoes(N, Tabuleiro):-
	length(Tabuleiro, N),
	maplist(same_length(Tabuleiro), Tabuleiro),
	
	append(Tabuleiro, Vars),
	domain(Vars, 0, 1),
	TotalPeoes #= 2 * N,
	sum(Vars, #=, TotalPeoes),
	
	transpose(Tabuleiro, Transposed),
	maplist(maxSum, Tabuleiro),
	maplist(maxSum, Transposed),
	
	labeling([ffc], Vars),
	maplist(writeL, Tabuleiro),nl.


maxSum(List):- sum(List, #=, Sum), Sum in 1..2.
writeL(List):- write(List), nl.



% 4.4)
% Artigos é a lista de sessões correspondentes ao artigo no index
% Participantes é um matriz binária em que as linhas são as sessões e as colunas os participantes, será 1 se o participante for à sessão
horarios(Artigos, Participantes):-
	findall(_Session, artigo(_, _, _), Artigos),
	sessoes(N, NPar),
	length(Participantes, N),
	%aborreci-me
	





sessoes(4, 2). % 4 Sessões no total, 2 sessões paralelas
areas([ia, rob, es, bd]). %4 Áreas temáticas
participantes([paulo,antonio,joao,pedro,ana,barbara,carla,carlos,diana,filipe]).
artigo(1, [paulo,antonio], [ia, rob]). % artigo 1 tem como autores o Paulo e o  António e áreas IA e Robótica
artigo(2, [paulo,joao], [ia, rob, es, bd]). %artigo 2 do Paulo/João foca 4 áreas
artigo(3, [pedro,joao], [ia, rob]).
artigo(4, [pedro, ana, barbara], [bd]).
artigo(5, [ana, carla, carlos], [es, bd]).
artigo(6, [carla, carlos, diana], [es, bd]).
artigo(7, [filipe], [ia, es]).
artigo(8, [diana, filipe], [es, bd]).
incompatibilidade(diana, paulo). % Diana não quer estar na mesma sessão do Paulo.
incompatibilidade(paulo, diana). % Paulo não quer estar na mesma sessão do Diana.
incompatibilidade(ana, pedro).
incompatibilidade(carla, filipe).
preferencias(paulo, [3,4]). % Paulo pretende assistir aos artigos 3 e 4 para
 além dos 1 e 2, dos quais é autor
preferencias(joao, []).
preferencias(antonio, [2,3]).
preferencias(ana, [6]).
preferencias(barbara, [6,7,8]). 



























