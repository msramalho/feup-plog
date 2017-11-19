:- use_module(library(lists)).
:- use_module(library(between)).
clear:-write('\33\[2J').

%participant(Id, Age, Performance)
participant(12, 17, 'Pé coxinho').
participant(34, 21, 'Programar com os pés').
participant(37, 20, 'Sing a Bit').
participant(48, 22, 'Pontes de esparguete').
participant(89, 19, 'Pontes de pen-drives').
participant(125, 20, 'Moodle hack').

%performance(Id, Times)
performance(12, [120, 120, 120, 120]).
performance(34, [32, 120, 45, 120]).
performance(37, [110, 2, 6, 43]).
performance(48, [120, 120, 110, 120]).
performance(89, [97, 101, 105, 110]).

%pergunta 1
madeItThrough(Participant):-
	performance(Participant, Times),
	member(120, Times).
	
%pergunta 2
getParticipantsJuriTime([], _, []).
getParticipantsJuriTime([P|R], JuriMember, [Value|Next]):-
	performance(P, Times),
	nth1(JuriMember, Times, Value),
	getParticipantsJuriTime(R, JuriMember, Next).

juriTimes(Participants, JuriMember, Times, Total):-
	getParticipantsJuriTime(Participants, JuriMember, Times),
	sumlist(Times, Total).
	
%pergunta 3
patientJuri(JuriMember):-
	performance(A, Times),
	nth1(JuriMember, Times, 120),
	performance(B, NewTimes),
	B\=A,
	nth1(JuriMember, NewTimes, 120).
	
%pergunta 4
getParticipantScores(P, Score):-
	performance(P, Times),
	sumlist(Times, Score).

bestParticipant(P1, P2, P):-
	getParticipantScores(P1, Score1),
	getParticipantScores(P2, Score2),
	Score1\=Score2,%different scores
	ite(Score1>Score2, P=P1, P=P2).
	
%pergunta 5
allPerfs:-
	performance(User, Times),
	participant(User, _, Performance),
	format('~d:~s:~p\n', [User, Performance, Times]),
	fail.
allPerfs.
	
%pergunta 6
nSuccessfulParticipants(T):-
	findall(Participant, (
		performance(Participant, Times), remove_dups(Times, [120])
	), Result),
	length(Result, T).
	
%pergunta 7
juriFans(JuriFansList):-
	findall(User-Fans,(
		performance(User, Times),
		findall(Fan, (nth1(Fan, Times, 120)), Fans)
	), JuriFansList).
	
%pergunta 8
eligibleOutcome(Id, Perf, TT):-
	performance(Id, Times),
	madeItThrough(Id),
	participant(Id, _, Perf),
	sumlist(Times, TT).

nextPhase(N, Participants):-
	setof(TT-Id-Perf, (eligibleOutcome(Id, Perf, TT)), All),
	nth1(N, All, _),%at least N
	length(Participants, N),
	append([Participants, _], All).
	
%pergunta 9
%pred(Idade, Participants, Performances)
%verdade se tem pelo menos um participante com idade menor ou igual a Q para cada performance, neste caso o cut é verde, porque só é útil para evitar backtracking, dado que não há necessidade de ir buscar um novo participante com idade menor ou igual a Q se não houve participantes suficientes para o anterior, já que também não haverá agora.
predX(Q, [R|Rs], [P|Ps]):-
	participant(R, I, P), I =< Q, !, 
	predX(Q, Rs, Ps).
predX(Q, [R|Rs], Ps):-
	participant(R, I, _), I > Q,
	predX(Q, Rs, Ps).
predX(_, [], []).
	
%pergunta 10
%impoe faz com que Mid seja uma lista com tamanho X, de seguida restringe a lista L a ser do formato [_, X, Mid, X, _], o que se traduz numa lista que começa e acaba em quaquer coisa, mesmo numa lista vazia, e que contem uma sublista que começa em X e acaba em X com X elementos pelo meio, respeitando a regra definida de subsquência válida para quando K é igual a X. Sendo que cada _ pode ser também uma lista vazia, ou seja L pode ser apenas [X, Mid, X] ou [_, X, Mid, X] ou [X, Mid, X, _]. Não estou a usar sintaxe de prolog porque Mid é uma lista e não é assim que se concatena, é só esquenática da ordem dos elementos/listas dentro de L.
impoe(X, L):-
	length(Mid, X),
	append(L1, [X|_], L),
	append(_, [X|Mid], L1).
	
%pergunta 11
unifyAll([], Final, Final).
unifyAll([UList|R], Acc, Final):-
	append([_, UList, _], Acc),
	unifyAll(R, Acc, Final).

langford(N, L):-
	Size is 2 * N, 
	length(LTemp, Size),
	range(1, N, Numbers),
	findall(SList, (
		member(X, Numbers),
		once(impoe(X, Temp)),
		once(append([_, Temp], SList))
	),SubSets),
	unifyAll(SubSets, LTemp, L).
	
	
	
	
	
	
	
	
	
	
	
%utils
ite(If, Then, _Else):- If, !, Then.
ite(_If, _Then, Else):- Else.
%range(I, F, Res):-findall(X, between(I, F, X), Res).


	
	