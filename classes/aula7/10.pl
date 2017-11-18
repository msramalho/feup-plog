:- use_module(library(lists)).
clear:-write('\33\[2J').

:- dynamic stepsTaken/1.

takeStep(Origin, Destination):-Destination is Origin + 1.
takeStep(Origin, Destination):-Destination is Origin + 2.

isFinal(Stairs, Stairs).
beforeEnd(New, Stairs):-New < Stairs.

calculateSteps(Stairs, Current, Steps):-
	takeStep(Current, New),
	isFinal(New, Stairs),
	assert(stepsTaken([New|Steps])),
	fail.
calculateSteps(Stairs, Current, Steps):-
	takeStep(Current, New),
	beforeEnd(New, Stairs),
	calculateSteps(Stairs, New, [New|Steps]).
	
casa_degraus(Stairs, _, _):-
	retractall(stepsTaken(_)),
	calculateSteps(Stairs, 0, []).
casa_degraus(_, Count, List):-
	findall(Step, stepsTaken(Step), List),
	length(List, Count).
	


	
	
	
	
	
	
	
	
	
	