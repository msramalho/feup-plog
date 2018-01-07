:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

%explanation - two matrices are constructed (one for each way the trip goes) this are boolean matrices where each line matches a bus line, but has as many elements as passengers in the bus. For a given line, the value of an index is 1 if passenger with the number matching the index is seated there. Afterwards, for all combinations of two lines between matrix1 and matrix2, the scalar_product is calculated and it is forced to be 1 or bellow, this means that the constraint of not going in the same row with someone in both trips is respected. 

seatdown(L, N, Seats1, Seats2):-
	length(L, NPersons),
	length(Seats1, NPersons), length(Seats2, NPersons), 
	domain(Seats1, 1, NPersons), domain(Seats2, 1, NPersons),
	%
	all_distinct(Seats1), all_distinct(Seats2),
	boolMatrix(N, Seats1, BM1),
	boolMatrix(N, Seats2, BM2),
	intersectMax1(BM1, BM2),
	%
	append(Seats1, Seats2, Seats),
	labeling([ffc], Seats),
	%
	show_seats(L, N, Seats1), nl, 
	show_seats(L, N, Seats2), nl.
	
intersectMax1([], _BM).
intersectMax1([L|T], BM):-
	intersectMax1(T, BM),
	intersectMax1_sub(L, BM).
	
intersectMax1_sub(_L, []).
intersectMax1_sub(L, [B|T]):-
	intersectMax1_sub(L, T),
	% scalar_product(L, B, #=<, 1). nao funciona com ambas indefinidas
	my_scalar_product(L, B, Total), 
	Total #=< 1.

my_scalar_product([],[], 0).
my_scalar_product([A|L], [B|R], Total):-
	my_scalar_product(L, R, AccTotal),
	Total #= AccTotal + A * B.
	
boolMatrix(N, Seats, BoolLine):-length(Seats, NTotal),boolMatrix(N, NTotal, Seats, BoolLine).
boolMatrix(N, NTotal, Seats, [CurrBoolLine|AccBoolLine]):-
	length(Row, N), % row has N persons
	append(Row, Rest, Seats), !,  %append will fail if there are less than N in Seats
	boolMatrix(N, NTotal, Rest, AccBoolLine),
	convertRowToBoolean(N, NTotal, Row, CurrBoolLine).
boolMatrix(_N, NTotal, Seats, [BoolLine]):-
	length(Seats, NSeats),
	convertRowToBoolean(NSeats, NTotal, Seats, BoolLine).
	
convertRowToBoolean(N, NTotal, Row, BoolLine):-
	length(BoolLine, NTotal),
	domain(BoolLine, 0, 1),
	count(1, BoolLine, #=, N),
	addRowToBoolLine(Row, BoolLine).
	
addRowToBoolLine([], _).
addRowToBoolLine([R|Row], BoolLine):- addRowToBoolLine(Row, BoolLine), element(R, BoolLine, 1).

% teacher predicate for printing to the screen
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