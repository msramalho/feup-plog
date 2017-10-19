/* This file implements the predicates required for displaying the board */
:- use_module(library(lists)).

:-include('utils.pl').

concatenateList([], Result, Result).
concatenateList([Color|NextColors], Initial, Final):-
	translate(Color, Value),
	atom_concat(Initial, Value, Result),
	concatenateList(NextColors, Result, Final).

%decides whether the next cell is valid or not and writes dashes if so or spaces if not
writeBetweenValid(LineIndex, CellIndex):-
    isValid(LineIndex, CellIndex),
    wd(5).
writeBetweenValid(LineIndex, CellIndex):-
    write('     ').

%final state of the state machine, enters it after first appearance of a valid cell
stateReceiveValidCell([], _, _).
stateReceiveValidCell([ColorsValid, _, ColorsUnknown|Rest], LineIndex, CellIndex):- % while a valid is passed stay in this state, has 3 elements, sort of a peek into the next two cells to see if dashes are an option
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


%initial state of the state machine, stays while there is no valid cell
stateIgnoreInvalidCell(Cells, LineIndex, CellIndex):-%leave state if a valid is found
	isValid(LineIndex, CellIndex),
	stateReceiveValidCell(Cells, LineIndex, CellIndex).

stateIgnoreInvalidCell([Cell|Rest], LineIndex, CellIndex):-%receive all the invalid
    write('     '),
	NewCellIndex is CellIndex + 1,
	stateIgnoreInvalidCell(Rest, LineIndex, NewCellIndex).


%alternate the kind of slashes according to even and odd lines
displaySlashes(LineIndex, CountValid):-
    0 is LineIndex mod 2, %even lines
    writeString('\\    /     ', CountValid).

displaySlashes(LineIndex, CountValid):- %odd lines
    writeString('/    \\     ', CountValid).

displaySlashesStart([], _, _).
displaySlashesStart([_|Rest], LineIndex, CellIndex):-
    isValid(LineIndex, CellIndex), !.
displaySlashesStart([_|Rest], LineIndex, CellIndex):-
    NewLineIndex is LineIndex + 1,
    displaySlashesStart(Rest, NewLineIndex, CellIndex),
    write('     ').

%handle the display of a line with slashes
displaySlashesLine(LineIndex):-
    NewLineIndex is LineIndex + 1,
    findall(X, isValid(NewLineIndex, X), L),
    length(L, CountValid),
    displaySlashes(LineIndex, CountValid).


%displays a line of the board and calls itself recursively
displayLine([], _).
displayLine([Line|Rest], LineIndex):-
	numberToString(LineIndex, LineString),	%convert the lineIndex to a string
	format('~2s  ', LineString),				%print the index with the right padding
	stateIgnoreInvalidCell(Line, LineIndex, 0), nl,
    displaySlashesStart(Line, LineIndex, 0),
    displaySlashesLine(LineIndex), nl,
	NewLineIndex is LineIndex + 1,
	displayLine(Rest, NewLineIndex).

%clears the screen and ouputs the board and the game state
displayBoard(Board, P1pieces, P2pieces, NowPlaying):-
	write('\33\[2J'),
	write('      0    1    2    3    4    5    6    7    8\n'),
	displayLine(Board, 0),
	wd(66),
	format('\nPlayer1: ~s, ~s', P1pieces),
	format('\nPlayer2: ~s, ~s', P2pieces),
	format('\nNow playing: ~s~3n', NowPlaying).



