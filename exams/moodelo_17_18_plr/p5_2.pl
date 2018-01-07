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
	cumlist(getMachine, PaperRolls, Machines, 1, _),
	cumlist(getTask, SelectedPaperRolls, Presents, Tasks, 1, _),
	cumulatives(Tasks, Machines, [bound(upper)]),
	
	% labeling
	labeling([ffc], SelectedPaperRolls).

getMachine(PaperRoll, machine(Index, PaperRoll), Index, NewIndex):- NewIndex #= Index + 1.

getTask(PaperRoll, Present, task(0, 1, 1, Present,PaperRoll), Index, NewIndex):-NewIndex #= Index + 1.









