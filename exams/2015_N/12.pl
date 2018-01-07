:-use_module(library(clpfd)).
clear:-write('\33\[2J').

% distribute([[1,2],[1,3],[2,3],[3],[2,3]], Vars).
% Vars = [1,3,2,3,2]
distribute(Domains, Vars):-
	%1 variables
	length(Domains, N), %number of items to distribute bags
	length(Vars, N),
	
	%2 domain
	domainVars(Domains, Vars), % uses list_to_fdset and in_set to give a domain to each element in Vars
	
	%3 restrictions
	noConsecutive(Vars),
	
	%4 labeling
	labeling([], Vars).
	
% domainVars(Domains, Vars).
domainVars([], []).
domainVars([Domain|D], [Var|V]):-
	list_to_fdset(Domain, DSet),
	Var in_set DSet,
	domainVars(D, V).
	
noConsecutive([_]).
noConsecutive([V1,V2|V]):-
	noConsecutive([V2|V]),
	V2 #\= V1.
	