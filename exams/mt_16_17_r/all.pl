:-use_module(library(lists)).

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
raro(Movie):-
    film(Movie, _, Duration, _),
    (Duration < 60; Duration > 120).

%pergunta 2
happierGuy(User1, User2, HappierGuy):-
    averageScore(User1, Score1),
    averageScore(User2, Score2),
    Score1 \= Score2, %cannot have the same average score
    ite(Score1 < Score2, HappierGuy = User2, HappierGuy = User1).

sumScore(List, Total):-sumScore(List, 0, Total).

sumScore([], Total, Total).
sumScore([_-Score|H], Acc, FinalTotal):-
    NewAcc is Acc + Score,
    sumScore(H, NewAcc, FinalTotal).

averageScore(User, AvgScore):-
    vote(User, Votes),
    length(Votes, Count),
    sumScore(Votes, Sum),
    AvgScore is Sum / Count.

%pergunta 3
splitScores([Movie-Score], [Score], [Movie]).
splitScores([Movie-Score|R], [Score|Scores], [Movie|Movies]):-splitScores(R, Scores, Movies).

getClean(User, Scores, Movies):-
    vote(User, DirtyScores),
    splitScores(DirtyScores, Scores, Movies).

likedBetter(User1, User2):-
    getClean(User1, CS1, _),
    getClean(User2, CS2, _),
    max_member(Max1, CS1),
    max_member(Max2, CS2),
    Max1 > Max2.%se o max1 não for maior nenhum do User1 será

%pergunta 4
%listDiff fails if not all of the first List is in L2
listDiff([], FinalDiff, FinalDiff).
listDiff([H|T], L2, FinalDiff):-
    nth0(_, L2, H, Diff), %if this fails H is not in L2
    listDiff(T, Diff, FinalDiff).

%%%%%%option 1 start
%returns repeated movies (all from all the similar users), so it's not the ideal solution:
recommendsRepeated(User, Movie):-
    getClean(User, _, UMovies),%user's movies
    user(Similar, _, _),
    User\=Similar, %not the same user
    getClean(Similar, _, SMovies),%potential similar's movies
    listDiff(UMovies, SMovies, MoviesDiff),
    member(Movie, MoviesDiff).

%however
%:-dynamic suggestions/2.
recommends(User, _Movie):-
    abolish(suggestions/2),
    assert(suggestions(User, [])),
    fail.

recommends(User, Movie):-
    recommendsRepeated(User, Movie),
    retract(suggestions(User, Previous)),
    append([Previous, [Movie]], NewList),
    assert(suggestions(User, NewList)),
    fail.
recommends(User, Movie):-
    retract(suggestions(User, Final)),
    remove_dups(Final, Pruned),
    member(Movie, Pruned).

%pergunta 5
invert(PredicateSymbol, Arity):-
    %:-dynamic PredicateSymbol/Arity,
    functor(P, PredicateSymbol, Arity),
    %retractall(P),
    %write(Elements),
    %retract(P),
    P,
    %assert(P),
    write(P), nl,
    %assert(P),
    fail.

%pergunta 6
%option1
onlyOne(User1, User2, Result):-
    getClean(User1, _, Movies1),
    getClean(User2, _, Movies2),
    findall(Movie, (member(Movie, Movies1), \+ nth0(_, Movies2, Movie)), In1Not2),
    findall(Movie, (member(Movie, Movies2), \+ nth0(_, Movies1, Movie)), In2Not1),
    append([In1Not2, In2Not1], Result).

/* % option2 - non deterministic solution
listAnd([], _L2, []).
listAnd([H1|T1], L2, [H1|Result]):-
    nth0(_, L2, H1, OptimizerList),%no need to send a list with the found values
    listAnd(T1, OptimizerList, Result).
listAnd([_|T1], L2, Result):-
    listAnd(T1, L2, Result).

onlyOne(User1, User2, Result):-
    getClean(User1, _, Movies1),
    getClean(User2, _, Movies2),
    append([Movies1, Movies2], OrMovies),
    remove_dups(OrMovies, OrMoviesUnique),
    %write('or: '), write(OrMovies), nl,
    listAnd(Movies1, Movies2, AndMovies),
    %write('and: '), write(AndMovies), nl,
    listDiff(AndMovies, OrMoviesUnique, Result).*/

%pergunta 7
:-dynamic filmUsersVotes/2.
filmVoting:-
    retractall(filmUsersVotes(_, _)),%clear the database
    film(Movie, _, _, _), %for each film
    findall(User-Vote, (vote(User, Votes), member(Movie-Vote, Votes)), UserVotes),
    assert(filmUsersVotes(Movie, UserVotes)), %save to the database
    %write(Movie), write(' - '), write(UserVotes), nl,
    fail.

%pergunta 8
writePredicates([], _).
writePredicates([Predicate-Arity|_], Stream):-
    functor(P, Predicate, Arity),
    P,
    write(Stream, P),
    write(Stream, '\n'),
    fail.
writePredicates([_|R], Stream):-writePredicates(R, Stream).

dumpDataBase(FileName):-
    open(FileName, write, File),
    writePredicates([user-3, film-4, vote-2], File),%flexible design
    close(File).

%utils
ite(If, Then, _Else):-If, !, Then.
ite(_If, _Then, Else):-Else.
clear:-write('\33\[2J').
