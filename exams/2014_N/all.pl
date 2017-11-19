:-use_module(library(lists)).

clear:-write('\33\[2J').

stat(ana, thresh, 1000, 200).
stat(ana, evelyn, 150, 200).
stat(joao, ezreal, 300, 40).
stat(joao, wukong, 100, 40).
stat(carlos, nidalee, 56, 60).
stat(carlos, thresh, 70, 6).

pref(ana, thresh).
pref(joao, wukong).
pref(carlos, nidalee).

%pergunta 1
%1.a
samePref(J1, J2):-
	pref(J1, Char),
	pref(J2, Char).%not needed: J1\=J2
%1.b
mostWinsWith(Char, Jog, Wins):-
	setof(W-J-L, stat(J, Char, W, L), Stats),
	%strange behaviour on setof
	write(Stats),
	last(Stats, Wins-Jog-_).
%1.c
statRatio(J, C, inf):-stat(J, C, _, 0).%infinite ratio
statRatio(J, C, R):-
	stat(J, C, Wins, Losses),
	R is Wins/Losses.
	
rightChoice(J):-
	pref(J, Char),
	setof(R-C, statRatio(J, C, R), Stats),
	last(Stats, _-BestChar),
	Char = BestChar.

%1.d
teamOfGoodChoices([]).
teamOfGoodChoices([J|T]):-
	rightChoice(J), teamOfGoodChoices(T).
	
%1.e
teamPrefs([], []).
teamPrefs([J|R], [Pref|Characters]):-
	pref(J, Pref),
	teamPrefs(R, Characters).	
	
%1.f
differentPrefs(Team):-
	teamPrefs(Team, Prefs),
	remove_dups(Prefs, Unique),
	length(Prefs, Len),
	length(Unique, Len).

%pergunta 2 - not tested
%2.a
n_vehicles(Num):-
	findall(V, vehicle(_, _, V, _), Vehicles),
	length(Vehicles, Num).
%2.b
authorized_vehicles(List):-
	findall(V, (
		vehicle(_, _, V, Date),
		stamp(V, _, End),
		today(Today),
		dif_days(End, Today, Diff),
		Diff >= 0
	), List).
%2.c
	%2.c.1
	%Recebendo um elemento este predicado sucessede se não for pessoa. Se esse elemento for pessoa apenas sucede se essa pessoa for maior. O nome sugerido seria algo como: maiorDeIdade(Pessoa), que sucede apenas se Pessoa tiver pelo menos 18 e falha se Pessoa não constar na lista de pessoas da base de factos
	
	%2.c.2
	%o cut é vermelho dado que se não existisse alteraria as soluções do predicado, na verdade, o predicado nunca falharia sem o cut.
	

%pergunta 3
childOf(mira,luis).
childOf(maria,carla).
childOf(maria,marco).
childOf(jose,marco).
childOf(jose,teresa).
childOf(marco,miguel).
childOf(joao,teresa).
childOf(joao,miguel).

%3.a
% dado uma pessoa X e uma pessoa Y,
% sucede se X for filho de Y em D grau, sendo que para N = 1, X é filho de Y; para N = 2, X é neto de Y e assim sucessivamente.
% resumindo verifica se Y é um ascendente de X e, se sim, em que grau (valor devolvido em D)
ascendente(X, Y, D):-ascendente(X, Y, D, []).
ascendente(X, Y, 1, _):-childOf(X, Y).
ascendente(X, Y, D, L):-
	childOf(X, Z), \+ member(Z, L),
	ascendente(Z, Y, D1, [Z|L]), 
	D is D1 + 1.
%3.b
relative(I, I, 1):-!.
relative(I1, I2, G):-ascendente(I1, I2, G), !.
relative(I1, I2, G):-ascendente(I2, I1, G), !.
relative(I1, I2, G):-
	ascendente(I1, Comum, D1),
	ascendente(I2, Comum, D2), !,
	G is D1 + D2.
relative(_, _, 0).
	

%pergunta 4
%PLR




	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	