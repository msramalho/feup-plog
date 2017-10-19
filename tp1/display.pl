/* This file implements the predicates required for displaying the board */
:- use_module(library(lists)).

:-include('utils.pl').

concatenateList([], Result, Result).
concatenateList([Color|NextColors], Initial, Final):-
	translate(Color, Value),
	atom_concat(Initial, Value, Result),
	concatenateList(NextColors, Result, Final).
/*

displayNextCell([], _, _).
displayNextCell([Colors|Rest], LineIndex, CellIndex):-%writes valid cells
	isValid(LineIndex, CellIndex),
	write('|'),
	concatenateList(Colors, '', Concated),
	format('~|~t~s~t~5+', Concated),
	write('|'),
	NewCellIndex is CellIndex + 1,
	displayNextCell(Rest, LineIndex, NewCellIndex).

displayNextCell([_|Rest], LineIndex, CellIndex):- %writes invalid cells
	write('|     |'),
	NewCellIndex is CellIndex + 1,
	displayNextCell(Rest, LineIndex, NewCellIndex).

groupLineCell([Colors|Rest], LineIndex, CellIndex):-
	isValid(LineIndex, CellIndex),

groupLineCell([_|Rest], LineIndex, CellIndex) %writes nothing just ignore and keep searching for valid
	NewCellIndex is CellIndex + 1,
	displayNextCell(Rest, LineIndex, NewCellIndex).
 */

writeBetweenValid(LineIndex, CellIndex):-
    isValid(LineIndex, CellIndex),
    wd(5).
writeBetweenValid(LineIndex, CellIndex):-
    write('     ').

stateReceiveValidCell([], _, _).

stateReceiveValidCell([ColorsValid, _, ColorsUnknown|Rest], LineIndex, CellIndex):- % while a valid is passed stay in this state, has 3 elements
	isValid(LineIndex, CellIndex),
	concatenateList(ColorsValid, '', Concatenated),
    format('~|~t~s~t~5+', Concatenated),
	NewCellIndex is CellIndex + 2,
    writeBetweenValid(LineIndex, NewCellIndex),
	stateReceiveValidCell([ColorsUnknown|Rest], LineIndex, NewCellIndex).

stateReceiveValidCell([PotentialLastColor|Rest], LineIndex, CellIndex):-
	isValid(LineIndex, CellIndex),
	concatenateList(PotentialLastColor, '', Concatenated),
    format('~|~t~s~t~5+', Concatenated),
	NewCellIndex is CellIndex + 1,
	stateReceiveValidCell(Rest, LineIndex, NewCellIndex).

stateReceiveValidCell([PotentialLastColor|Rest], LineIndex, CellIndex):-
    write('     '),
	NewCellIndex is CellIndex + 1,
	stateReceiveValidCell(Rest, LineIndex, NewCellIndex).



stateIgnoreInvalidCell(Cells, LineIndex, CellIndex):-%leave state if a valid is found
	isValid(LineIndex, CellIndex),
	stateReceiveValidCell(Cells, LineIndex, CellIndex).

stateIgnoreInvalidCell([Cell|Rest], LineIndex, CellIndex):-%receive all the invalid
    write('     '),
	NewCellIndex is CellIndex + 1,
	stateIgnoreInvalidCell(Rest, LineIndex, NewCellIndex).



displayLine([], _).
displayLine([Line|Rest], LineIndex):-
	numberToString(LineIndex, LineString),	%convert the lineIndex to a string
	format('~2s  ', LineString),				%print the index with the right padding

	stateIgnoreInvalidCell(Line, LineIndex, 0),
    %listToString(Result, StringResult),
    %write(Result),
    nl,
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



