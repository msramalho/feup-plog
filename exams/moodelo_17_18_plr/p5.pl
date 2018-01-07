:-use_module(library(clpfd)).
:-use_module(library(lists)).

clear:-write('\33\[2J').

wrap(Presents, PaperRolls, SelectedPaperRolls):-
	% variables
	same_length(SelectedPaperRolls, Presents),
	
	% domain
	length(PaperRolls, NRolls),
	domain(SelectedPaperRolls, 1, NRolls),
	
	% constraints
	cumlist(getRectangles(PaperRolls), Presents, SelectedPaperRolls, 0, Rectangles),
	disjoint2(Rectangles),
	
	maplist(fixSum(PaperRolls), Rectangles),
	
	% labeling
	maplist(rectStarts, Rectangles, Starts),
	append(Starts, SelectedPaperRolls, Vars),
	labeling([ffc], Vars).
	% maplist(writeL,Rectangles).
	
rectStarts(wrap(S,_,_,_),S).

fixSum(PaperRolls, wrap(X, Present, SPR, 1)):-
	element(SPR, PaperRolls, Max),
	X + Present #=< Max.
	

getRectangles(PaperRolls, Present, SPR, _, wrap(X, Present, SPR, 1)):-
	element(SPR, PaperRolls, PaperRoll),
	X + Present #=< PaperRoll,
	X #>= 0.
	
writeL(L):-write(L),nl.











