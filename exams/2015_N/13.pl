:-use_module(library(clpfd)).
clear:-write('\33\[2J').

% distribute(3, [[1,2],[1,3],[2,3],[3],[2,3]], 2, 3, Vars).
% Vars = [1,3,2,3,2]
distribute(NBags, Domains, MinObj, MaxObj, Vars):-
	%1 variables
	length(Domains, N), %number of items to distribute bags
	length(Vars, N),
	
	%2 domain
	domainVars(Domains, Vars), % uses list_to_fdset and in_set to give a domain to each element in Vars
	
	%3 restrictions
	nvalue(TempNBags, Vars), % at max NBags
	TempNBags #=< NBags,
	countAll(NBags, Vars, MinObj, MaxObj),
	
	%4 labeling
	labeling([], Vars).
	
% domainVars(Domains, Vars).
domainVars([], []).
domainVars([Domain|D], [Var|V]):-
	list_to_fdset(Domain, DSet),
	Var in_set DSet,
	domainVars(D, V).
	
countAll(0, _, _, _).
countAll(Count, Vars, MinObj, MaxObj):-
	Res in MinObj..MaxObj #\/ Res #= 0,
	% Res #>= MinObj #/\ Res #=< MaxObj #\/ Res #=0,
	count(Count, Vars, #=, Res),
	NewCount is Count - 1,
	countAll(NewCount, Vars, MinObj, MaxObj).
	
	
	
	
	
	
	
	
	
	
	