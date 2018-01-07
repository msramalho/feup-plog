:-use_module(library(clpfd)).
:-use_module(library(lists)).

clear:-write('\33\[2J').

build(Budget, NPacks, ObjectCosts, ObjectPacks, ObjectsIndexes, UsedPacks):-
	% variables
	same_length(ObjectCosts, Objects),
	
	% domain
	domain(Objects, 0, 1),
	
	% constraints
	count(1, Objects, #=, 3),
	scalar_product(ObjectCosts, Objects, #=, Cost),
	Cost in 0.. Budget,
	scalar_product(ObjectPacks, Objects, #=, UsedPacks),
	UsedPacks in 0.. NPacks,
	
	% labeling
	labeling([maximize(UsedPacks),down], Objects),
	findall(Index, nth1(Index, Objects, 1), ObjectsIndexes).