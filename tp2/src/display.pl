%------------------------------------printing results
writeResult(Teachers, Subjects, Heuristic, CountPracticalUndesiredTeacher, RatioFailedHours):-
	write('\n-------------------------------------------DONE\n'),
	debug('Teachers:\n'), debugList(Teachers),
	debug('Subjects:\n'), debugList(Subjects),

	writeTeachersResult(Teachers),
	writeSubjectsResult(Subjects),
	% due to the FD in clpfd
	HeuristicPercent is Heuristic / 10000,
	FailedHoursPercent is RatioFailedHours / 10000,
	write('For the next 3 values "0" represents a perfect score for the given area:'), nl,
	format('Heuristic: ~2f%', [HeuristicPercent]), nl,
	format('Failed Hours: ~2f%', [FailedHoursPercent]), nl,
	format('#Practical Teacher From Other Field: ~p', [CountPracticalUndesiredTeacher]), nl,
	nl, nl.

% pretty teachers results (table)
writeTeachersResult(Teachers):-
	length(Teachers, NTeachers),
	format('\nTeachers Distribution (~p)', [NTeachers]), nl,
	format('~` t~4|~`*t~37|\n', []),
	format('~` t~4|* ID  FIELD AVG  DIFF  HS1  HS2 *\n', []),
	writeTeachersResult(Teachers, 1),
	format('~` t~4|~`*t~37|\n', []).
writeTeachersResult([], _).
writeTeachersResult([Avg-Diff-Field-HS1-HS2|T], Counter):-
	format('~` t~4|*~t~d~t~6+~t~d~t~5+~t~d~t~6+~t~d~t~6+~t~p~t~4+~t~p~t~5+*~38|~n', [Counter, Field, Avg, Diff, HS1, HS2]),
	% format('*~t~d~t~30+*~31|~n', [Counter]), % example of simple centering
	NewCounter is Counter + 1,
	writeTeachersResult(T, NewCounter).

% pretty subjects results (table)
writeSubjectsResult(Subjects):-
	length(Subjects, NSubjects),
	format('\nSubjects Distribution (~p)', [NSubjects]), nl,
	format('~` t~4|~`*t~98|\n', []),
	format('~` t~4|* ID  Field  Semester  HT  DT  HP  DP   Theoretical(Tid-Hours)      Practical(Tid-Hours)     *\n', []),
	writeSubjectsResult(Subjects, 1),
	format('~` t~4|~`*t~98|\n', []).

writeSubjectsResult([], _).
writeSubjectsResult([[Semester-Field-HT-HP-DT-DP, TT, TP]|S], Counter):-
	findall(TIndex-THours, (nth1(TIndex, TT, THours), THours \= 0), Tteachers),
	findall(PIndex-PHours, (nth1(PIndex, TP, PHours), PHours \= 0), Pteachers),
	format('~` t~4|*~t~d~t~6+~t~d~t~6+~t~d~t~12+~t~p~t~1+~t~p~t~5+~t~p~t~5+~t~p~t~3+~t~p~t~25+~t~p~t~30+*~91|~n', [Counter, Field, Semester, HT, DT, HP, DP, Tteachers, Pteachers]),
	NewCounter is Counter + 1,
	writeSubjectsResult(S, NewCounter).

