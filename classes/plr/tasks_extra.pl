:- use_module(library(lists)).
:- use_module(library(clpfd)).

clear:-write('\33\[2J').



tarefas(Starts, Ends):-
    % 1 - variáveis
    Starts = [S1, S2, S3, S4, S5, S6, S7],
    Ends = [E1, E2, E3, E4, E5, E6, E7],
    LTasks = [
        task(S1, 16, E1, 2, 1),
        task(S2, 6, E2, 9, 2),
        task(S3, 13, E3, 3, 3),
        task(S4, 7, E4, 7, 4),
        task(S5, 5, E5, 10, 5),
        task(S6, 18, E6, 1, 6),
        task(S7, 4, E7, 11, 7)
    ],

    % 2- domain
    domain(Starts, 0, 40),
    % 3 - restrictions
    cumulative(LTasks, [limit(13)]),
    maximum(MaxEnd, Ends),

    % 4 - label
    labeling([minimize(MaxEnd)], Starts).
