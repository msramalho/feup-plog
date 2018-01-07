:-use_module(library(clpfd)).
% :-use_module(library(lists)).
clear:-write('\33\[2J').

% (Start, Duration, Worth)
% attend([(1,3,2),(1,4,4),(4,4,3)], G, W)
attend(FilmList, Goings, Worth):-
	%variables
	length(FilmList, NFilms),
	length(Goings, NFilms),

	%domains
	domain(Goings, 0, 1), % false or true
	
	%constraints
	findall(Worth, member((_,_,Worth), FilmList), Worths),
	scalar_product(Worths, Goings, #=, Worth),
	createTastks(FilmList, Goings, Tasks),
	
	Machines = [machine(0, 99999), machine(1, 1)],
	cumulatives(Tasks, Machines, [bound(upper)]),
	
	%labeling
	labeling([maximize(Worth)], Goings).

createTastks([], [], []).
createTastks([(S, D, _)|F], [G|Goings], [Task|Tasks]):-
	Task = task(S, D, _, 1, G),
	createTastks(F, Goings, Tasks).

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
% restrictOverlaps([_], _, [], _, _Goings).
% restrictOverlaps([_First, Second|ToGo], I, [], _J, Goings):- 
	% NewI is I + 1,  NewJ is NewI + 1,
	% restrictOverlaps([Second|ToGo], NewI, ToGo, NewJ, Goings).
% restrictOverlaps([F1|Films], I, [F2|T], J, Goings):-
	% write(I-J), nl,
	% NewJ is J + 1, 
	% restrictOverlaps([F1|Films], I, T, NewJ, Goings),
	% element(I, Goings, G1), element(J, Goings, G2), 
	% unify
	% F1 = [S1, D1, _], F2 = [S2, D2, _],
	% E1 is S1 + D1,
	% E2 is S2 + D2,
	% ite(
		% ((S1 < S2, E1 > S2); (S2 < S1, E2 > S1)),
		% ((G1 #= 1 #=> G2 #= 0) #\/ (G2 #= 1 #=> G1 #= 0)),
		% (G1 #= G1)
	% ).
	
% ite(If, Then, _Else):- If, write('then'), Then. 
% ite(_If, _Then, Else):- write('else'), Else. 
	
	


% getMovies([], [], []).
% getMovies([_|T], [Going|G], Movies):-
	% getMovies(T, G, AccMovies),
	% Going #<=> 0, AccMovies = Movies.
% getMovies([(S,D,_)|T], [Going|G], Movies):-
	% getMovies(T, G, AccMovies),
	% Going #<=> 1,
	% append([movie(S,D)], AccMovies, Movies).
	
% L =[m(1,3),m(1,4),m(4,4)], disjoint1(L),labeling([],L).
	
	
	
	
	
	
	
	
	
	

	