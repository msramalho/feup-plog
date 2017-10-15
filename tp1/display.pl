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
	displayLineIndex(LineIndex), write(' '),
	displayNextCell(Line, LineIndex, 0),nl,
	NewLineIndex is LineIndex + 1,
	displayLine(Rest, NewLineIndex).


displayBoard(Board, P1pieces, P2pieces, NowPlaying):-
	displayLine(Board, 0),
	write('-------------------------------------------------------'),
	format('\nPlayer1: ~s, ~s', P1pieces),
	format('\nPlayer2: ~s, ~s', P2pieces),
	format('\nNow playing: ~s~3n', NowPlaying).



