:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

seatdown(L, N, Seats1, Seats2):-
	length(L, NPersons),
	length(Seats1, NPersons), length(Seats2, NPersons), 
	domain(Seats1, 1, NPersons), domain(Seats2, 1, NPersons),
	%
	all_distinct(Seats1), all_distinct(Seats2),
	different_n(N, Seats1, Seats2),
	%
	append(Seats1, Seats2, Seats),
	labeling([], Seats),
	%
	show_seats(L, N, Seats1), nl, 
	show_seats(L, N, Seats2), nl.
	
different_n(N, Seats1, Seats2):-
	length(ListN, N),
	append(ListN, Others1, Seats1),
	!,
	all_different_n(ListN, Seats2),
	different_n(N, Others1, Seats2).
different_n(_N, Seats1, Seats2):-all_different_n(Seats1, Seats2).%when append fails
	
all_different_n([], _Seats2).
all_different_n([X|ListN], Seats2):-
	all_different_n(ListN, Seats2),
	all_different_n_inner(X, ListN, Seats2).

all_different_n_inner(_X, [], _Seats2).
all_different_n_inner(X1, [X2|ListN], Seats2):-
	all_different_n_inner(X1, ListN, Seats2),
	all_different_n_inner_s2(X1, X2, Seats2).

all_different_n_inner_s2(_X1, _X2, []).
all_different_n_inner_s2(X1, X2, [Y|Seats2]):-
	all_different_n_inner_s2(X1, X2, Seats2),
	all_different_n_inner_s2_inner(X1, X2, Y, Seats2).

all_different_n_inner_s2_inner(_X1, _X2, _Y1, []).
all_different_n_inner_s2_inner(X1, X2, Y1, [Y2|Seats2]):-
	all_different_n_inner_s2_inner(X1, X2, Y1, Seats2),
	(X1 #\= Y1 #/\ X1 #\= Y2) #\/ (X2 #\= Y1 #/\ X2 #\= Y2).
	
show_seats(Ppl,N,Seats):-
	length(Row, N), append(Row, SeatsSufix, Seats),
	!,
	write_people(Ppl, Row),
	show_seats(Ppl, N, SeatsSufix).
show_seats(Ppl, _, Seats):-write_people(Ppl, Seats).

write_people(Ppl, List):-
	get_ppl(Ppl, List, ListPpl), write(ListPpl), nl.
	
get_ppl(_, [], []).
get_ppl(L, [X|Xs], [P|Ps]):-
	nth1(X,L,P),
	get_ppl(L,Xs,Ps).
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	