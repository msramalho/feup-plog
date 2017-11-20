:-use_module(library(lists)).
clear:-write('\33\[2J').

%pergunta 1

% prof_risco(Pessoa,Profissão)
prof_risco(rui,professor).
prof_risco(to,bombeiro).
prof_risco(marta,bombeiro).
prof_risco(hugo,fuzileiro).

% fuma_muito(Pessoa)
fuma_muito(sofia).
fuma_muito(rui).
fuma_muito(marta).
fuma_muito(pedro).

% dorme(Pessoa,Horas)
dorme(pedro,5).
dorme(joao,4).
dorme(marta,5).
dorme(sofia,8).
dorme(hugo,4).

%pergunta 1
%1.a
dorme_pouco(Pessoa):-
	dorme(Pessoa, H),
	H < 6.

%1.b
morre_cedo(Pessoa):-
	(prof_risco(Pessoa, _));
	(fuma_muito(Pessoa), dorme_pouco(Pessoa)).

%1.c
%morre_cedo(P). 
%rui, to, marta, hugo, marta, pedro, no.

%1.d
sortudo(Pessoa):-
	prof_risco(Pessoa, Prof),
	prof_risco(SegundaPessoa, Prof),
	Pessoa\=SegundaPessoa.

desgracado(Pessoa):- 
	prof_risco(Pessoa, _),
	\+ sortudo(Pessoa).

%1.e
getPessoa(Pessoa):-
	prof_risco(Pessoa, _);
	fuma_muito(Pessoa);
	dorme(Pessoa, _).
detalhes(P, Profissao, FumaMuito, DormePouco, MorreCedo):-
	getPessoa(P),
	ite(prof_risco(P, Pr), Profissao = Pr, Profissao = '-'),
	ite(fuma_muito(P), FumaMuito = 'x', FumaMuito = ' '),
	ite(dorme_pouco(P), DormePouco = 'x', DormePouco = ' '),
	ite(morre_cedo(P), MorreCedo = 'x', MorreCedo = ' ').
	

listagem:-%should hav used findall to avoid duplicate, but that's just wasting time now
	nl, 
	detalhes(Pessoa, Profissao, FumaMuito, DormePouco, MorreCedo),
	format('~15s ~15s ~15s ~15s ~15s\n', [Pessoa, Profissao, FumaMuito, DormePouco, MorreCedo]),
	fail.

	
%pergunta 2
%nascimento(IdPessoa,IdMãe,Data)
nascimento(10, 20, dia0).
nascimento(3, 20, dia1).
nascimento(4, 30, dia1).
%óbito(IdPessoa,Data)
%casamento(IdConjuge1,IdConjuge2,Data)
%divórcio(IdPessoa1,IdPessoa2,Data)
%imigrou(IdPessoa,Data)

%2.a
quantidade_de_registos_de_nascimento(N):-
	findall(Id, nascimento(Id, _, _), Ids),
	length(Ids, N).

%2.b
primeiro_nativo(IdPessoa):-
	bagof(Id, Data^Mae^nascimento(Id, Mae, Data), Ids),
	write(Ids),
	[IdPessoa|_] = Ids.	

%2.c
vivoAData(Pessoa, Data):-
	obito(Pessoa, DataObito), !, antes(DataObito, Data).
vivoAData(_Pessoa, _Data).

populacao_total(Data, QtPessoas):-%not tested, needs antes(D1, D2).
	findall(Pessoa, (
		(nascimento(Pessoa, _, _); imigrou(Pessoa)),
		vivoAData(Pessoa, Data)
	),Habitantes),
	length(Habitantes, QtPessoas).

%2.d
%idade(Data, Idade):- ... .%falta
maeSolteira(Id):-
	%filho menor
	setof(Idade, Filho^(
		nascimento(Filho, Id, Nascimento),
		idade(Nascimento, Idade)
	), Filhos),%filhos ordenados por idade crescente
	[MaisNovo-_|_] = Filhos,
	MaisNovo < 18,
	%nao casada
	findall(Marido, (casamento(Id, Marido, _)), Maridos),
	findall(Ex, divorcio(Id, Ex, _), Exs),
	length(Maridos, Count), length(Exs, Count). %tantos casamentos como divorcios
	
maes_solteiras(Ids):-%falta menor(Data)
	findall(Id, (maeSolteira(Id)), Ids).


%3
%3.a
%o predicado a (agrupa) devolve, dada uma lista de elementos, uma lista de listas em que cada uma dessas listas é composta por elementos iguais, ou seja, agrupa elementos iguais. Contudo apenas agrupa os que estão seguidos, por exemplo: [1, 1, 1, 2, 1] resultaria em [[1, 1, 1], [2], [3]].
%o cut é vermelho pois previne a separação de dois elementos iguais e consecutivos no backtracing.
%termina se só restar um elemento e junta ao acumulador
agrupa([X], Xs, [[X|Xs]]).
%Se vierem dois elementos iguais, passa o segundo e adiciona ao acc
agrupa([X,X|R], Xs, L2):-
	!, 
	agrupa([X|R], [X|Xs], L2).
%se os dois primeiros forem diferentes, ignora o primeiro e passa ao próximo um acumulador vazio, mas guardando no resultado a lista dos repetidos que vinha de trás
agrupa([X,Y|R], Xs, [[X|Xs]|RL]):-
	agrupa([Y|R], [], RL).
	
%3.b
%stop and return a solution
leitura([], Leitura, Leitura).
%recursive solutions will be given through this
leitura([], Acc, Leitura):-leitura_de(Leitura, Acc).
%generate a reading from a grouped list
leitura([Grupo|R], Acc, Leitura):-
	length(Grupo, Len),
	[Elem|_] = Grupo,
	append([Acc, [Len, Elem]], NewAcc),
	leitura(R, NewAcc, Leitura).
	
leitura_de(Leitura, Anterior):-
	agrupa(Anterior, [], Agrupado),
	leitura(Agrupado, [], Leitura).
	
	

	
	
	
	
	









	
	
	
	
	
	
	
ite(If, Then, _Else):- If, !, Then.
ite(_If, _Then, Else):- Else.