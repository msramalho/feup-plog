:- use_module(library(lists)).
clear:-write('\33\[2J').

%participant(Id,Age,Performance)
participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programar com os pés').
participant(3788, 20, 'Sing a Bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle hack').

%performance(Id,Times)
performance(1234,[120,120,120,120]).
performance(3423,[32,120,45,120]).
performance(3788,[110,2,6,43]).
performance(4865,[120,120,110,120]).
performance(8937,[97,101,105,110]).

%pergunta 1
madeItThrough(Participant):-
	performance(Participant, Times),
	member(120, Times).

%pergunta 2
juriTimes([], _JuriMember, [], 0).
juriTimes([Participant|R], JuriMember, [Time|Times], Total):-
	performance(Participant, PTimes),
	nth1(JuriMember, PTimes, Time),
	juriTimes(R, JuriMember, Times, OldTotal),
	Total is OldTotal + Time.

%pergunta 3
patientJuri(JuriMember):-
	performance(A, Times),
	nth1(JuriMember, Times, 120),
	performance(B, NewTimes),
	B\=A,
	nth1(JuriMember, NewTimes, 120).


%pergunta 4
getBest(_P1, _P2, Sum, Sum, _):- !, fail.
getBest(P1, _P2, Sum1, Sum2, P1):- Sum1 > Sum2.
getBest(_P1, P2, _Sum1, _Sum2, P2).
bestParticipant(P1, P2, P):-
	performance(P1, Times1),
	performance(P2, Times2),
	sumlist(Times1, Sum1),
	sumlist(Times2, Sum2),
	getBest(P1, P2, Sum1, Sum2, P).

%pergunta 5
allPerfs:-
	performance(Participant, Times), %performance comes before to exclude as soon as possible
	participant(Participant, _, What),
	format('~p:~p:~p\n',[Participant, What, Times]),
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
	findall(Participant-Juris, (
		performance(Participant, Times),
		findall(Juri, (nth1(Juri, Times, 120)),Juris)
	), JuriFansList).


%pergunta 8
%given predicate
eligibleOutcome(Id,Perf,TT) :-
    performance(Id,Times),
    madeItThrough(Id),
    participant(Id,_,Perf),
    sumlist(Times,TT).

nextPhase(N, Participants):-
	length(Participants, N),
	setof(TT-Participant-Perf, (eligibleOutcome(Participant, Perf, TT)), AllReversed),
	reverse(AllReversed, All),
	append(Participants, _, All).
	
%pergunta 9
predX(_, [], []).
predX(Q, [R|Rs], [P|Ps]):-
	participant(R, I, P), 
	I =< Q, !, 
	predX(Q, Rs, Ps).
predX(Q, [R|Rs], Ps):-
	participant(R, I, _), 
	I > Q, %desnecessário
	predX(Q, Rs, Ps).
%Este predicado recebe uma idade e uma lista de participantes, devolvendo, no terceiro parâmetro, uma lista das atuações correspondentes (ordenadas de acordo com a lista de participantes (apenas os escolhidos)) aos participantes escolhidos; são escolhidos os participantes e, consequentemente as suas atuações, que têm idade menor ou igual a Q.
%De realçar que o uso de I > Q é desencessário
%O cut utilizado é verde (dado que a cláusula I > Q existe) porque não altera as soluções geradas, apenas previne a procura de soluções inexistentes.


%pergunta 10
impoe(X, L):-
	length(Mid, X),
	append(L1, [X|_], L),
	append(_, [X|Mid], L1).
%Ao longo da resposta '_' significa Qualquer Coisa: um elemento, um número, uma lista, uma lista vazia, nada, ...
%Este predicado recebe uma lista L e um número X.
%O predicado altera a lista L de forma a que seja uma lista não completamente instanciada e que respeite as condições:
%1. começa com _
%2. acaba com _
%3. tem, entre o início (_) e o fim (_) uma lista de comprimento X + 2, que começa e acaba com o número X e que tem, de permeio, tantos elementos quanto o valor de X.
%No fundo algo como [_, X, X_ELEMENTOS_NAO_INSTANCIADOS, X, _], onde X_ELEMENTOS_NAO_INSTANCIADOS é uma sequência de X elementos não instanciados.

%pergunta 11
impoeTodos(0, _).
impoeTodos(N, L):-
	impoe(N, L1),
	N1 is N - 1,
	impoeTodos(N1, L).

langford(N, L):-
	Size is 2 * N,
	length(L, Size),
	impoeTodos(N, L).