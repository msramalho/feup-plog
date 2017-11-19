:-use_module(library(lists)).
:-use_module(library(statistics)).
:-use_module(library(between)).
:-use_module(library(sets)).

clear:-write('\33\[2J').

%alowed starting times
hours([7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5]).
%airTime(TvShow,DayOfWeek,Hour)
airTime('The Walking Dead',sunday,9).
airTime('Game of Thrones',sunday,8.5).
airTime('The Big Bang Theory',monday,8).
airTime('How I Met Your Mother',thursday,8).
airTime('Mad Men',sunday,10).

%views(TvShow,MillionsOfViews)
views('The Walking Dead',11).
views('Game of Thrones',5).
views('The Big Bang Theory',9).
views('Mad Men',2.5).
views('How I Met Your Mother',19).

%network(TvShow,Network).
network('The Walking Dead',amc).
network('Mad Men',amc).
network('Game of Thrones',hbo).
network('The Big Bang Theory',cbs).
network('How I Met Your Mother',cbs).

%pergunta 1
%1.a
tvShowNetwork(Network, DayOfWeek, Hour, TvShow):-
	network(TvShow, Network),
	airTime(TvShow, DayOfWeek, Hour), !.
	
%1.b (rever)
:-dynamic shows/1.
loadNetworkShows(Network):-
	network(TvShow, Network),
	views(TvShow, MillionsOfViews),
	retract(shows(S)),
	NewS = [MillionsOfViews-TvShow|S],
	assert(shows(NewS)),
	fail.
loadNetworkShows(_).
getNetworkShows(Network, Shows):-
	retractall(shows(_)),
	assert(shows([])),
	loadNetworkShows(Network),
	retract(shows(Shows)).
	
mostViews(Network, TvShow, DayOfWeek, Hour):-
	getNetworkShows(Network, Shows),
	max_member(_-TvShow, Shows), !, 
	airTime(TvShow, DayOfWeek, Hour).
	
%1.c
hottestTvShows(Networks, TvShows):-hottestTvShows(Networks, [], TvShows).
hottestTvShows([], TvShows, TvShows).
hottestTvShows([Network|T], Acc, TvShows):-
	mostViews(Network, TvShow, _, _),
	append(Acc, [TvShow], NewAcc),
	hottestTvShows(T, NewAcc, TvShows).
	
%1.d
hourHasShow([], _, _, Final, Final).
hourHasShow([Hour|R], Network, DayOfWeek, Acc, Final):-
	network(TvShow, Network),
	airTime(TvShow, DayOfWeek, Hour),
	append([Acc, [TvShow]], NewAcc),
	hourHasShow(R, Network, DayOfWeek, NewAcc, Final).
hourHasShow([_|R], Network, DayOfWeek, Acc, Final):-
hourHasShow(R, Network, DayOfWeek, Acc, Final).
	
schedule(Network, DayOfWeek, Schedule):-
	hours(Hours),
	hourHasShow(Hours, Network, DayOfWeek, [], Schedule), !.
	
%1.e
:-dynamic showViews/1.
getTvShowByNetworkDay(Network, DayOfWeek):-
	network(TvShow, Network),
	airTime(TvShow, DayOfWeek, _),
	views(TvShow, Views),
	retract(showViews(Current)),
	append([Current, [Views]], New),
	assert(showViews(New)),
	fail.
getTvShowByNetworkDay(_,_).
getAllViews(Network, DayOfWeek, Views):-
	assert(showViews([])),
	getTvShowByNetworkDay(Network, DayOfWeek),
	retract(showViews(Views)).
	
averageViewers(Network, DayOfWeek, Average):-
	getAllViews(Network, DayOfWeek, List),
	write(List),
	mean(List, Average).

%pergunta 2
%project(ProjID,Name).
%task(ProjID,TaskId,Description,NecessaryTime).
%precedence(ProjID,TaskId1,TaskId2). % precedência entre tarefas (TaskId1->TaskId2)

%2.a
proj_tasks(L):-
	findall(Proj-Len, (
		project(Proj, _),
		findall(Task, (task(Proj, Task, _, _)), Tasks),
		length(Tasks, Len)
	), L).

%2.b
project(projA,qwe).

task(projA,t1,a,3).
task(projA,t2,b,2).
task(projA,t3,c,4).
task(projA,t4,d,2).

precedence(projA,t1,t2).
precedence(projA,t1,t3).
precedence(projA,t2,t4).
precedence(projA,t3,t4).
%o predicado percorre um caminho pelas tarefas, começando na inicial (que não tem nenhuma precedência), passando por um conjunto sequencial de tarefas (em que de uma tarefa A passa para B sendo que B depende de A), até chegar a uma tarefa terminal (não tem nenhuma tarefa que dela dependa) - no processo de backtracking é calculada a duração total deste caminho do projeto e devolvido em D e também constrói a sequência de tarefas visitads em P.
%fazer p(projA, P, D) devolve uma solução de t1 até t4, se pedirmos todas as soluçõe teremos, pela ordem apresentada:
% P = [t1, t2, t4], D = 7.
% P = [t1, t3, t4], D = 9.
p(X, P, D):-
	task(X, Y, _, _), %carrega uma tarefa do projeto X
	\+ precedence(X, _, Y), % que não tem precedência
	p(X, Y, P, D). %invoca P\4 a partir do inicial
p(X, Y, [Y|P], D):-%adiciona, durante backtracking, Y à lista
	precedence(X, Y, Z), %Z precisa de Y
	task(X, Y, _, D1), % lê a duração de Y
	p(X, Z, P, D2), % invoca P\4 a partir para Z
	D is D1 + D2. %D é dado pela duração do atual e pela dos seguintes (está somada em D2)
p(X, Y, [Y], D):-
	\+ precedence(X, Y, _), %chegou a uma task final
	task(X, Y, _, D). %devolve a duração desta

total_time(Proj, TotalTime):-
	findall(D, p(Proj, _, D), Durations),
	max_member(TotalTime, Durations).

%pergunta 3
	
createGroup(Acc, _, _, GroupSum, _Group):-
	sumlist(Acc, Sum),
	Sum > GroupSum, !, fail.
createGroup(Group, Ns, Ns, GroupSum, Group):-
	sumlist(Group, GroupSum).
createGroup(Acc, Ns, NewNs, GroupSum, Group):-
	nth0(_, Ns, Number, TempNs),%select a number from Ns
	createGroup([Number|Acc], TempNs, NewNs, GroupSum, Group).

createGroups([], [], _).
createGroups([Group|T], Ns, GroupSum):-
	createGroup([], Ns, NewNs, GroupSum, Group),
	createGroups(T, NewNs, GroupSum).

grupos(N, K, Grupos):-
	numlist(1, N, R),
	reverse(R, Ns),
	sumlist(Ns, Sum),
	GroupSum is integer(Sum/K),
	length(Grupos, K),
	createGroups(Grupos, Ns, GroupSum).
	
	
	
	
	
	




















