:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

objeto(piano, 3, 30).
objeto(cadeira, 1, 10).
objeto(cama, 3, 15).
objeto(mesa, 2, 15).
homens(4).
tempo_max(60).

moving(Tasks):-
	homens(Limit),
	tempo_max(MaxTime),
	findall([Name, Cost, Dur], objeto(Name, Cost, Dur), Objects),
	cumlist(getTask(MaxTime), Objects, Tasks, 1, _),
	cumulative(Tasks, [limit(Limit)]),
	maplist(getEnd, Tasks, Ends),
	% maximum(Last, Ends), labeling([minimize(Last), ffc],Ends). % optimal choice
	% labeling([ffc],Ends).
	
getTask(MaxTime, [_,Cost,Dur], task(_, Dur, End, Cost, Id), Id, NextId):- NextId #= Id + 1, End in Dur..MaxTime.

getEnd(task(_,_,End,_,_), End).