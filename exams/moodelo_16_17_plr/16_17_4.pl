:-use_module(library(clpfd)).
:-use_module(library(lists)).
clear:-write('\33\[2J').

% sweet_recipes(60, 30, [20,50,10,20,15],[6,4,12,20,6], Cookings, Eggs).
% sweet_recipes(120, 30, [20,50,10,20,15],[6,4,12,20,6], Cookings, Eggs).
sweet_recipes(MaxTime, NEggs, RecipeTimes, RecipeEggs, Cookings, Eggs):-
	%variables
	length(RecipeTimes, NRecipes),
	length(Pratos, NRecipes),
	
	%domains
	domain(Pratos, 0, 1),
		
	%constraints
	count(1, Pratos, #=, 3), %Numero fixo de pratos
	scalar_product(RecipeTimes, Pratos, #=<, MaxTime),
	scalar_product(RecipeEggs, Pratos, #=, Eggs),
	Eggs #=< NEggs,
	
	%labeling
	labeling([maximize(Eggs)], Pratos),
	findall(Index, nth1(Index, Pratos, 1), Cookings).
	