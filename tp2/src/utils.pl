/*
    Contains useful and context-free predicates
*/
clear:-write('\33\[2J').


%generate an fdset with a start, end and step
generateFdset(Min, Max, Step, Fdset):-
    generateList(Min, Max, Step, List),
    list_to_fdset(List, Fdset).
%generate a list with a start, end and step
generateList(Max, Max, _Step, [Max]).
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