/*
    Contains useful and context-free predicates
*/
clear:-write('\33\[2J').



% DEBUG MODE FUNCTIONS wether this is in debug mode or not
:- volatile debugMode/1.
:- dynamic debugMode/1.
:- assert(debugMode(false)).
setDebug:-retract(debugMode(_)),assert(debugMode(true)).
clearDebug:-retract(debugMode(_)),assert(debugMode(false)).
% only write message if debug mode is on
debug(Message):-debugMode(true), write(Message), !.
debug(_).
% only write list if debug mode is on
debugList(List):-debugMode(true), writeList(List).
debugList(_).

%generate an fdset with a start, end and step
generateFdset(Min, Max, Step, Fdset):-
    generateList(Min, Max, Step, List),
    list_to_fdset(List, Fdset).
%generate a list with a start, end and step
generateList(Max, Max, _Step, [Max]):-!.
generateList(MoreThanMax, Max, _Step, []):-MoreThanMax > Max, !.
generateList(Min, Max, Step, NewList):-
    NewMin is Min + Step,
    generateList(NewMin, Max, Step, List),
    NewList = [Min|List].

%apply a domain restriction to every element of a list, namely in_set
domainFdset([], _).
domainFdset([N|R], Fdset):-
    N in_set Fdset,
    domainFdset(R, Fdset).

%sum two n-size lists into another
scalarSum([], [], []).
scalarSum([E1|T1], [E2|T2], [Sum|TSum]):-
    scalarSum(T1, T2, TSum),
    Sum #= E1 + E2.

%apply scalarSum to a list of lists
scalarSumMatrix([], []).
scalarSumMatrix([Last], Last).
scalarSumMatrix([L1|R], Sum):-
    scalarSumMatrix(R, TempSum),
    scalarSum(L1, TempSum, Sum).

%restrict lists to match element by element
restrictEqualLists([], []):-!.
restrictEqualLists([A|R1], [B|R2]):-
    A #= B,
    restrictEqualLists(R1, R2).

%initialize a list of size N full of E
emptyList([], 0, _):-!.
emptyList([E|L], N, E):-
    N1 is N - 1,
    emptyList(L, N1, E).

%writeList
writeList([]).
writeList([E|R]):-
    write(E), nl,
    writeList(R).

% statistics measurement
resetWalltime:-statistics(walltime, _).
debugWalltime:-debugMode(true), statistics(walltime, [_, W]), format('Walltime: ~d\n', W).
debugWalltime.
debugStatistics:-debugMode(true), fd_statistics.
debugStatistics.
/* resetRuntime(Start):-statistics(runtime, [Start|_]).
writeRuntime(Start):-statistics(runtime, [End|_]),
	Runtime is End - Start,
	format('Runtime: ~3d\n', Runtime). */