/* This file implements the predicates required for displaying the board */
:- use_module(library(lists)).

concatenateList([], Result, Result).
concatenateList([Color|NextColors], Initial, Final):-
	translate(Color, Value),
	atom_concat(Initial, Value, Result),
	concatenateList(NextColors, Result, Final).


displayNextCell([], _, _).
displayNextCell([Colors|Rest], LineIndex, CellIndex):-
	isValid(LineIndex, CellIndex),
	write('|'),
	concatenateList(Colors, '', Concated),
	format('~|~t~s~t~5+', Concated),
	write('|'),
	NewCellIndex is CellIndex + 1,
	displayNextCell(Rest, LineIndex, NewCellIndex).

displayNextCell([_|Rest], LineIndex, CellIndex):-
	write('|     |'),
	NewCellIndex is CellIndex + 1,
	displayNextCell(Rest, LineIndex, NewCellIndex).

displayLineIndex(LineIndex):-
	LineIndex < 10,
	write(' '),
	write(LineIndex).
displayLineIndex(LineIndex):-
	write(LineIndex).

displayLine([], _).
displayLine([Line|Rest], LineIndex):-
	numberToString(LineIndex, LineString),	%convert the lineIndex to a string
	format('~2s ', LineString),				%print the index with the right padding

	displayNextCell(Line, LineIndex, 0),nl,
	NewLineIndex is LineIndex + 1,
	displayLine(Rest, NewLineIndex).


displayBoard(Board, P1pieces, P2pieces, NowPlaying):-
	write('\33\[2J'),
	write('      0    1    2    3    4    5    6    7    8\n'),
	displayLine(Board, 0),
	format('~66c', "-"),
	format('\nPlayer1: ~s, ~s', P1pieces),
	format('\nPlayer2: ~s, ~s', P2pieces),
	format('\nNow playing: ~s~3n', NowPlaying).



