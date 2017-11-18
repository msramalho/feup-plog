:- use_module(library(lists)).
:- dynamic film/4, movimento/1.
%Factos

film('A', [action, adventure, fantasy], 115, 7.6).
film('B', [biography, drama, romance], 131, 8.7).
film('C', [action, adventure, crime], 121, 6.4).
film('D', [drama, mystery, scifi], 116, 8.5).
film('E', [action, crime, drama], 127, 7.6).
film('F', [drama, mystery, thriller], 112, 6.7).

user(john, 1992, 'USA').
user(jack, 1989, 'UK').
user(peter, 1983, 'Portugal').
user(harry, 1993, 'USA').
user(richard, 1982, 'USA').

vote(john, ['C'-7, 'A'-9, 'E'-6]).
vote(jack, ['C'-8, 'A'-8, 'E'-7]).
vote(peter, ['E'-4, 'B'-7, 'F'-3]).
vote(harry, ['C'-7, 'E'-6]).
vote(richard, ['C'-10, 'B'-10, 'D'-9]).


%pergunta 1
curto(Movie):-
	film(Movie, _, Duration, _),
	Duration < 125.

%pergunta 2
diff(User1, User2, Difference, Film):-
	vote(User1, Movies1),
	member(Film-Score1, Movies1),
	vote(User2, Movies2),
	member(Film-Score2, Movies2),
	Difference is abs(Score1 - Score2).

%pergunta 3
clearVotes([], [], []).
clearVotes([Movie-Score|T], [Movie|Movies], [Score|Scores]):-
	clearVotes(T, Movies, Scores).
/*
%option 1, not optimal due to max_member O(n)
niceGuy(User):-
	vote(User, Votes),
	clearVotes(Votes, _, Scores),
	max_member(Max, Scores),
	Max >= 8.
*/

checkNiceGuy([_-Score|_]):-Score >= 8.
checkNiceGuy([_|Votes]):-checkNiceGuy(Votes).
niceGuy(User):-%option 2
	vote(User, Votes),
	checkNiceGuy(Votes).

%pergunta 4
/*
%option 1
elemsComuns([], [], _).
elemsComuns([E1|T1], [E1|Common], List2):-
	memberchk(E1, List2), !,
	elemsComuns(T1, Common, List2).
elemsComuns([_|T1], Common, List2):-
	elemsComuns(T1, Common, List2).
*/
%option 2 (option with better performance)
elemsComuns([], [], _).
elemsComuns([E1|T1], [E1|Common], List2):-
	nth0(_, List2, E1, RequiredOnly), !,
	elemsComuns(T1, Common, RequiredOnly).
elemsComuns([_|T1], Common, List2):-
	elemsComuns(T1, Common, List2).

%pergunta 5
printCategory(Category):-
	film(Movie, Categories, Duration, Score),
	member(Category, Categories),
	format('~s (~dmin, ~1f/10)', [Movie, Duration, Score]), nl,
	fail.
printCategory(_Category).%the yes at the end

%pergunta 6
similarity(Film1, Film2, Similarity):-
	film(Film1, Categories1, Duration1, Score1),
	film(Film2, Categories2, Duration2, Score2),
	%categorias comuns
	elemsComuns(Categories1, AndCategories, Categories2),
	length(AndCategories, NumCommon),
	%categorias distintas entre os dois
	append([Categories1, Categories2], TempCategories),
	remove_dups(TempCategories, OrCategories),
	length(OrCategories, NumDistinct),
	%formula
	DurDiff is abs(Duration1 - Duration2),
	ScoreDiff is abs(Score1 - Score2),
	PercentCommonCat is (NumCommon/NumDistinct)*100,
	Similarity is PercentCommonCat - 3 * DurDiff - 5 * ScoreDiff.
	%TempSimilarity is PercentCommonCat - 3 * DurDiff - 5 * ScoreDiff,
	%Similarity is integer(TempSimilarity * 10)/10.

%pergunta 7
mostSimilar(Film, MaxS, Films):-
	bagof(S, (similarity(Film, F2, S), Film \= F2), [MaxS]),
	MaxS > 10, !,
	findall(F3, similarity(Film, F3, MaxS), Films).
mostSimilar(_Film, 0, []).

%pergunta 8

getClean(User, Movies, Scores):-
	vote(User, Votes),
	clearVotes(Votes, Movies, Scores).
sumDiffs([], 0).
sumDiffs([X-Y|R], NewSum):-
	sumDiffs(R, Sum),
	NewSum is Sum + abs(X - Y).

averageDiffs(Diffs, Avg):-
	sumDiffs(Diffs, Sum),
	length(Diffs, Count),
	Avg is abs(Sum/Count).

getCountryDiff(User1, User2, 0):-
	user(User1, _, Country1),
	user(User2, _, Country1).
getCountryDiff(_User1, _User2, 2).

getAgeDiff(User1, User2, AgeDiff):-
	user(User1, Year1, _),
	user(User2, Year2, _),
	AgeDiff is abs(Year1 - Year2).

distancia(User1, Distancia, User2):-
	getClean(User1, Movies1, _),
	getClean(User2, Movies2, _),
	elemsComuns(Movies1, MoviesCommon, Movies2),
	findall(S1-S2,
		(
			member(Movie, MoviesCommon),
			vote(User1, M1),
			member(Movie-S1, M1),
			vote(User2, M2),
			member(Movie-S2, M2)
		), Differences),
	averageDiffs(Differences, AvgDiff),
	getCountryDiff(User1, User2, CountryDiff),
	getAgeDiff(User1, User2, AgeDiff), !,
	Distancia is AvgDiff + AgeDiff/3 + CountryDiff.

%pergunta 9
/*
	vote(john, ['C'-7, 'A'-9, 'E'-6]).
	vote(jack, ['C'-8, 'A'-8, 'E'-7]).
film('A', [action, adventure, fantasy], 115, 7.6).
*/
update(Film):-
	findall(Score, (vote(_User, Votes), member(Film-Score, Votes)), Scores),
	sumlist(Scores, Sum),
	length(Scores, Count),
	NewScore is Sum / Count,
	retract(film(Film, Categories, Duration, _)),
	assert(film(Film, Categories, Duration, NewScore)).


%pergunta 10
averageUserScore(User, AverageScore):-
	vote(User, Votes), !,
	findall(Score, member(_Film-Score, Votes), Scores),
	length(Scores, CountScores),
	sumlist(Scores, SumScores),
	AverageScore is SumScores/CountScores.
%cut vermelho dado que altera o funcionamento expectável das funções do tipo findall, sendo que faz com que estas só consigam devolver um resultado - o do primeiro user listado.


%pergunta 11
validCoordinate(Coord):-Coord >= 1, Coord =< 8.
validPosition(X/Y):-validCoordinate(X), validCoordinate(Y).
moveTo(L/C, Lf/Cf):-
	Available = [[1,2],[2,1],[-1,2],[2,-1],[1,-2],[-2,1],[-1,-2],[-2,-1]],
	member([DifX,DifY], Available),
	Lf is L + DifX,
	Cf is C + DifY,
	validPosition(Lf/Cf).

move(L/C, Celulas):-findall(X/Y, moveTo(L/C, X/Y), Celulas).

%pergunta 12
nextMoves([], _).
nextMoves([L/C|R], N):-
	assert(movimento(L/C)),
	N1 is N -1,
	nextMoves(R, N),
	podeMoverEmN(L/C, N1).

podeMoverEmN(_, 0).
podeMoverEmN(L/C, N):-
	findall(X/Y, moveTo(L/C, X/Y), Celulas),
	nextMoves(Celulas, N).

podeMoverEmN(L/C, N, Celulas):-
	podeMoverEmN(L/C, N),
	findall(Mov, movimento(Mov), DuplicateCelulas),
	remove_dups(DuplicateCelulas, Celulas),!,
	retractall(movimento(_)).

%pergunta 13 - BFS
minJogadas(Li/Ci, Lf/Cf, Min):-
	processMoves([Li/Ci], Lf/Cf, 1, Min).
%find all children of a list of nodes
getNextMoves([], Moves, Moves).
getNextMoves([L/C|R], Acc, Moves):-
	findall(X/Y, moveTo(L/C, X/Y), Celulas),
	append([Celulas, Acc], TempMoves),
	getNextMoves(R, TempMoves, Moves).
	
%check if the solution is in this level
chegouFim([Lf/Cf|_], Lf/Cf, Min, Min).
chegouFim([_|R], Lf/Cf, Acc, Min):-chegouFim(R, Lf/Cf, Acc, Min).

%breadth-first checks level and only moves forward if it fails
bfs(Moves, Lf/Cf, Acc, Min):-chegouFim(Moves, Lf/Cf, Acc, Min), !.
bfs(Moves, Lf/Cf, Acc, Min):-%if the solution is not in this level
	NewAcc is Acc +1,
	processMoves(Moves, Lf/Cf, NewAcc, Min).%next level
	
processMoves(Moves, Lf/Cf, Acc, Min):-
	getNextMoves(Moves, [], Children),
	bfs(Children, Lf/Cf, Acc, Min).
	

%breadth first search (generic)
child(a, b).
child(a, c).
child(b, d).
child(b, e).
child(e, h).
child(c, f).
child(c, g).

checkTerminal(g).

parseLevel([], FinalQueue, FinalQueue).
parseLevel([Node|Nodes], AccQueue, FinalQueue):-
    findall(X, child(Node, X), Children),
    append([AccQueue, Children], Queue),
    parseLevel(Nodes, Queue, FinalQueue).

checkNodes([Node|_Nodes]):-
    format('checking: ~p \n', [Node]),
    checkTerminal(Node).
checkNodes([_Node|Nodes]):-checkNodes(Nodes).

checkLevel(Queue):-
    checkNodes(Queue).
checkLevel(Queue):-
    bfs(Queue).
bfs(Nodes):-
    parseLevel(Nodes, [], Queue),
    checkLevel(Queue).
%get children, for each child add their to queue




















%utils
clear:-write('\33\[2J').