:-use_module(library(clpfd)).
clear:-write('\33\[2J').

% cut([12,50,14,8,10,90,24], [100,45,70], S).
cut(Shelves, Boards, SelectedBoards):-
	%variables
	length(Shelves, NShelves),
	length(SelectedBoards, NShelves), % index = shelve, value = board index
	
	%domain
	length(Boards, NBoards),
	domain(SelectedBoards, 1, NBoards),
	
	%constraints
	getTasks(Shelves, SelectedBoards, Tasks),
	getMachines(Boards, Machines, 1),
	cumulatives(Tasks, Machines, [bound(upper)]),
	
	%labeling
	labeling([], SelectedBoards).
	
getTasks([], [], []).
getTasks([S|Shelves], [B|SelectedBoards], [Task|Tasks]):-
	Task = task(0, S, S, S, B),
	getTasks(Shelves, SelectedBoards, Tasks).
	
getMachines([], [], _).
getMachines([B|Boards], [M|Machines], Id):-
	M = machine(Id, B),
	NewId is Id + 1, 
	getMachines(Boards, Machines, NewId).