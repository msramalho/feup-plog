:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(between)).
clear:-write('\33\[2J').

% emparelhar homens e mluheres, respeitar o delta de diferença máxima de altura, maximizar pares, 
% homem é sempre mais alto que mulher

% | ?- gym_pairs([75, 85, 68, 70], [65, 76, 60, 80], 10, Pairs).                                            
% no

% | ?- gym_pairs([75, 85, 68, 70], [65, 76, 60, 70], 10, Pairs).                                              
% Pairs = [1-4,2-2,3-1,4-3] ? 
% yes

% | ?- gym_pairs([75, 85, 68, 70], [65, 74, 60, 80], 10, Pairs).                                              
% Pairs = [1-2,2-4,3-1,4-3] ? ;
% Pairs = [1-2,2-4,3-3,4-1] ? ;
% no

gym_pairs(MenHeights, WomenHeights, Delta, Pairs):-
	length(MenHeights, N),
	same_length(Hs, Ms, N),
	domain(Hs, 1, N),
	domain(Ms, 1, N),
	all_distinct(Hs),
	all_distinct(Ms),
	scanlist(heightRule(MenHeights, WomenHeights, Delta), Hs, Ms, 0, _),
	Hs = [First|Rest],
	scanlist(sortIt, Rest, First, _), % no symmetries
	
	append(Hs, Ms, Vars),
	labeling([], Vars),
	keys_and_values(Pairs, Hs, Ms).
	
	
heightRule(MenHeights, WomenHeights, Delta, Hi, Mi, 0, 0):-
	element(Hi, MenHeights, H),
	element(Mi, WomenHeights, M),
	H #> M #/\ H - M #=< Delta.
	
sortIt(Current, Prev, Current):- Prev #< Current.



% | ?- optimal_gym_pairs([75, 85, 68, 70], [65, 76, 60, 70], 10, Pairs).                                      
% Pairs = [1-4,2-2,3-1,4-3] ? ;
% no

% | ?- optimal_gym_pairs([75, 85, 68, 70], [65, 76, 60, 80], 10, Pairs).                                      
% Pairs = [1-1,2-2,3-3] ? ;
% no
optimal_gym_pairs(MenHeights, WomenHeights, Delta, Pairs):-
	same_length(Matrix, MenHeights),
	maplist(same_length(WomenHeights), Matrix),
	maplist(domainAndConstrain, Matrix),
	transpose(Matrix, TMatrix),
	maplist(domainAndConstrain, TMatrix),
	scanlist(heightRules(MenHeights, WomenHeights, Delta), Matrix, 1, _),
	scanlist(sumLine, Matrix, 0, CountPairs),
	append(Matrix, Vars),
	labeling([maximize(CountPairs),down], Vars),
	findall(H-M, (nth1(H, Matrix, Line), nth1(M, Line, 1)), Pairs).
	
heightRules(MenHeights, WomenHeights, Delta, Line, Hi, NextHi):-
	NextHi #= Hi + 1,
	scanlist(heightRuleCell(MenHeights, WomenHeights, Delta, Hi), Line, 1, _).
	
heightRuleCell(MenHeights, WomenHeights, Delta, Hi, Cell, Mi, NextMi):-
	NextMi #= Mi + 1,
	element(Hi, MenHeights, H),
	element(Mi, WomenHeights, M),
	Cell #=> H #> M #/\ H - M #=< Delta.

domainAndConstrain(Line):-
	domain(Line, 0, 1),
	count(1, Line, #=, Match),
	Match in 0..1. %single match

sumLine(Line, Prev, Sum):- sum(Line, #=, Acc), Sum #= Prev + Acc.










