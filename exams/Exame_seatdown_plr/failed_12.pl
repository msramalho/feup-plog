:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

seatdown(L, N, Seats1, Seats2):-
	length(L, NPersons),
	length(Seats1, NPersons), length(Seats2, NPersons), 
	domain(Seats1, 1, NPersons), domain(Seats2, 1, NPersons),
	%
	all_distinct(Seats1), all_distinct(Seats2),
	different_Ns(N, Seats1, Seats2),
	%
	write(Seats1), nl, 
	append(Seats1, Seats2, Seats),
	labeling([], Seats),
	%
	show_seats(N, L, Seats1), nl, 
	show_seats(N, L, Seats2), nl.
	
different_Ns(_,[], _).
different_Ns(N, X, _Y):-length(X, LenX), LenX =< N.
different_Ns(_, [_], _).
different_Ns(N, X, Y):-
	length(ListN, N),
	append(ListN, ListX, X),!,
	% #\ append([_, ListN, _], Y),
	different_N(ListN, Y), %ListN cannot occur in Y
	different_Ns(N, ListX, Y).
	
different_N(_, []).
different_N(ListN, Y):-length(Y, LenY), length(ListN, N), LenY =< N.
different_N(ListN, Y):-
	length(ListN, N), %read size
	length(ListYN, N),%new list with that size
	append(ListYN, ListY, Y), !,%get the first N elements of Y in ListYN
	differentLists(ListN, ListYN, 1), % cannot be the same
	% (X1 #\= Y1 #/\ X1 #\= Y2) #\/ (X2 #\= Y1 #/\ X2 #\= Y2),
	different_N(ListN, ListY).
	
	
differentLists([], _, 1).
differentLists([E1|L1], L2, Res):-
	differentLists(L1, L2, AccRes),
	differentListsElement(E1, L2, NewRes),
	AccRes #\/ NewRes #<=> Res.
	
differentListsElement(_, [], 1).
differentListsElement(Element, [E2|L2], Res):-
	differentListsElement(Element, L2, AccRes),
	(AccRes #/\ Element #\= E2) #<=> Res.
	
show_seats(_N, _,[]).
show_seats(_N, L,[X]):-
	nth1(X, L, Person), write(Person), nl.
show_seats(N, L, X):-
	length(N, ListN),
	append(ListN, _ListX, X),!,
	writePerson(L, ListN, ToWrite),
	write(ToWrite),
	show_seats(N,L,X).
	
writePerson(_, [], _).
writePerson(L, [X1|P], ToWrite):-
	writePerson(L, P, AccToWrite),
	nth1(X1, L, Person),
	ToWrite = AccToWrite-Person.

	
	
	
	
	
	
	
	
	
	
	
	
	
	