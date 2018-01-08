:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% 7.1)
getSum(Sum):-
	Vars = [X, Y, Z],
	domain(Vars, 0, 9),
	Y #\= 0,
	Z #\= 0,
	Top #= 10 * Y + X,
	Result #= 100 * Z + 10 * X + X,
	Sum #= X + Y + Z,
	Top * 7 #= Result,
	labeling([], Vars).
	
% 7.2)
% versao disjoint1
docentesd(Starts):-
	findall([Name, Sex, From, To], docente(Name, Sex, From, To), Docentes),
	cumlist(getClass, Docentes, Classes, Starts, 0, WomenStarts),
	disjoint1(Classes),
	sum(WomenStarts, #=, Cost),
	labeling([minimize(Cost)], Starts),
	write(Classes).
	
getClass([_, m, From, To], class(Start, 1), Start, X, X):- Start in From..To.
getClass([_, f, From, To], class(Start, 1), Start, X, NewX):- Start in From..To, NewX #= X + Start.

% versao cumulative
docentesc(Starts):-
	findall([Name, Sex, From, To], docente(Name, Sex, From, To), Docentes),
	cumlist(getTask, Docentes, Tasks, Starts, 1, _),
	cumulative(Tasks, []),
	maplist(getWomenStart(Docentes), Tasks, WomenStarts),
	sum(WomenStarts, #=, Cost),
	labeling([minimize(Cost)], Starts),
	write(Tasks).

getTask([_, _, From, To], task(Start, 1, _, 1, Id), Start, Id, NextId):- NextId #= Id + 1, Start in From..To.
	
getWomenStart(Docentes, task(Start,_,_,_,Id), WomenStart):-
	nth1(Id, Docentes, [_, Sex, _, _]),
	((Sex = m, WomenStart = 0);
	(Sex = f, WomenStart = Start)).
	
docente(pedro, m, 3, 6). % Pedro é do sexo masculino e pode leccionar aulas que comecem às 3, 4, 5 ou 6 horas
docente(joana, f, 3, 4).
docente(ana, f, 2, 5).
docente(joao, m, 2, 4).
docente(david, m, 3, 4).
docente(maria, f, 1, 6). 