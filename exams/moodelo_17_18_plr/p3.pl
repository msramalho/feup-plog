:- use_module(library(clpfd)).
:- use_module(library(lists)).

clear:-write('\33\[2J').
p3(L1,L2) :-
    length(L1,N),
    length(L2,N),
    %
	same_length(L1,Is),
    pos(L1,L2,Is),
    all_distinct(Is),
    %
    test(L2),
    labeling([],Is).

pos([],_,[]).
pos([X|Xs],L2,[I|Is]):-
    element(I,L2,X),
    pos(Xs,L2,Is).
	
test([_,_]).
test([X1,X2,X3|Xs]):-
    (X1 #< X2 #/\ X2 #< X3 #\/ X1 #> X2 #/\ X2 #> X3),
    test([X2,X3|Xs]).