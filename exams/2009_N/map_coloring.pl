:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% Colors será uma lista com as cores de cada pais 1 a N, pais(N)
map(Colors, Score):-
	% variables
	cores(NCores, CountCores), % teorema diz que bastarão sempre 4
	paises(NPaises), 
	length(Colors, NPaises),
	
	% domain
	domain(Colors, 1, NCores),

	% constraints
	adjacentesCoresDiferentes(Colors),% adjacentes + diferentes
	cumlist(maxCountColor(Colors), CountCores, 1, _), % cardinalidade
	exclui(Colors), % exclusao
	ifIncludes(Colors), ifExcludes(Colors), % condicionais
	
	% labeling
	Score in 0..NPaises,
	getScore(Colors, Scores), % preferencias
	last(Scores, Score),
	append(Colors, [Score], Vars),
	labeling([maximize(Score)],Vars).

adjacentesCoresDiferentes(Colors):-
	findall(P1, F1^fronteira(P1, F1), PaisesFronteira1),
	remove_dups(PaisesFronteira1, NDPaisesFronteira1),
	write(NDPaisesFronteira1), nl,
	findall(P2-Fronteiricos, (member(P2,NDPaisesFronteira1), findall(F2, (fronteira(P2, F2); diferentes(P2, F2)),Fronteiricos)), Fronteiras),
	write(Fronteiras), nl,
	maplist(difColors(Colors), Fronteiras).
difColors(Colors, Pais-Fronteiras):-
	element(Pais, Colors, Cor),
	maplist(difColor(Colors, Cor), Fronteiras).
difColor(Colors, Cor, Fronteira):-
	element(Fronteira, Colors, CorDiff), 
	Cor #\= CorDiff.
	
maxCountColor(Colors, MaxCount, Index, NextIndex):-
	NextIndex #= Index + 1,
	count(Index, Colors, #=<, MaxCount).

exclui(Colors):-
	findall(Pais-Exclui, (exclui(Pais, LExclui), list_to_fdset(LExclui, Exclui)), ExcluiCores),
	maplist(excluiCor(Colors), ExcluiCores).
excluiCor(Colors, Pais-Exclui):-
	element(Pais, Colors, Cor),
	Temp in_set Exclui,
	Cor #\= Temp.

ifIncludes(Colors):-
	findall(SP-SC-EP-EC, seentao(SP-SC, EP-EC), SeEntao),
	maplist(ifInclude(Colors), SeEntao).
ifInclude(Colors, SP-SC-EP-EC):-
	element(SP, Colors, SCor),
	element(EP, Colors, ECor),
	SCor #= SC #=> ECor #= EC.
	
ifExcludes(Colors):-
	findall(SP-SC-EP-EC, seexclui(SP-SC, EP-EC), SeEntao),
	maplist(ifExclude(Colors), SeEntao).
ifExclude(Colors, SP-SC-EP-EC):-
	element(SP, Colors, SCor),
	element(EP, Colors, ECor),
	SCor #= SC #=> ECor #\= EC.

getScore(Colors, Score):-
	preferencias(Preferences),
	cumlist(colorScore(Colors), Preferences, 0, Score).
colorScore(Colors, P-Prefs, Score, NewScore):-
	element(P, Colors, Cor),
	list_to_fdset(Prefs, SPrefs),
	Preference in_set SPrefs,
	Cor #= Preference #<=> NewScore #= Score + 1,
	Cor #\= Preference #<=> NewScore #= Score.
	

paises(5).
cores(4, [2,1,1,2]). % 4 cores, 1 e 4 usadas 2 vezes no max e 2 e 3 uma vez no max

fronteira(1,2).
fronteira(1,3).
fronteira(1,4).
fronteira(1,5).
fronteira(2,3).
fronteira(2,4).
fronteira(3,4).
fronteira(4,5).

exclui(1, [3,4]). % pais 1 não pode ter cores 3 e 4
exclui(2, [3]).

seentao(1-4, 2-3). % Se 1 tiver cor 4 #=> 2 tem cor 3
seexclui(1-3, 2-1). % Se 1 tiver cor 3 #=> 2 tem cor 1
diferentes(2,5). % embora não façam fronteira, têm cores diferentes

preferencias([1-[1,2], 2-[1], 3-[3,4], 4-[1,2]]). % pais 1 tem preferencia pelas cores 1 e 2, pais 2 pela 1, pais 3 pela 3 e 4, pais 4 pela 1 e 2
