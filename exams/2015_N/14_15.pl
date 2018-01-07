:-use_module(library(clpfd)).
clear:-write('\33\[2J').

%cost is incremented by the distance of the selected session for a teacher in the preferences list
% Pref is a list of preferences for a teacher
% session is irrelevant
cost(Prefs, _Sessions, Schedule, Cost):-cost(Prefs, Schedule, Cost).
cost([], [], 0).
cost([PrefO|Prefs], [ScheduleO|Schedule], Cost):-
	cost(Prefs, Schedule, AccCost),
	element(NewCost, PrefO, ScheduleO), !,  %if the real is in the preferences then the index is used
	Cost #= AccCost + NewCost - 1.
cost([PrefO|Prefs], [_ScheduleO|Schedule], Cost):-
	cost(Prefs, Schedule, AccCost),
	length(PrefO, NewCost),  % if the real is not in the preferences then the index is used assume cost = length + 1
	Cost #= AccCost + NewCost + 1.


conf_schedule(Prefs, Sessions, Schedule):-
	%1 vars
	length(Prefs, Speakers),
	length(Schedule, Speakers),
	%2 domain
	length(Sessions, MaxSession),
	domain(Schedule, 1, MaxSession),
	%3 restrictions
	restrictMaxPresentations(1, Sessions, Schedule),
	%4 labeling
	cost(Prefs, Sessions, Schedule, Cost),
	labeling([minimize(Cost)], Schedule).
	
restrictMaxPresentations(_SIndex, [], _Schedule).
restrictMaxPresentations(SIndex, [Session|S], Schedule):-
	NewSIndex is SIndex + 1,
	restrictMaxPresentations(NewSIndex, S, Schedule),
	count(NewSIndex, Schedule, #=<, Session).
	
	
	
	
	