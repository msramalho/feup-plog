:- use_module(library(lists)).
:- use_module(library(clpfd)).

clear:-write('\33\[2J').

mailman(L, Sum):-
    % 1 - variables
    length(L, 10), % 10 houses

    % 2 - domain
    domain(L, 1, 10), % house numbers
    all_distinct(L), %

    % 3 - restrict
    element(10, L, 6),
    sumDiffs(L, Sum),

    % 4 - label
    labeling(maximize(Sum), L).
    % labeling([], L).

init(Res):- % to use this remove the maximize condition
    setof(Sum-L, mailman(L, Sum), TempRes),
    reverse(TempRes, Res).

sumDiffs([_], 0).
sumDiffs([E1, E2|T], Sum):-
    sumDiffs([E2|T], TempSum),
    Sum #= abs(E1 - E2) + TempSum.