:- use_module(library(random)).
:- use_module(library(lists)).

/*The board is a matrix of 13 lines and 9 lines, not all are used */
emptyBoard([
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]],
    [[empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty], [empty]]
]).

/* Available pieces to place in the board*/
availablePiecesToPlace([
    ['D', 8],
    ['R', 8],
    ['I', 8],
    ['G', 8],
    ['B', 8],
    ['W', 3]
]).

replace(New, Old, [Old | Resto], [New | Resto]).
replace(New, Old, [Same | Resto1 ], [Same | Resto2 ]):-
	Same \= Old,
	replace(New, Old, Resto1, Resto2).


replace([Find|T], Find, Replace, [Replace|T]).
replace([Head|T1], Find, Replace, []):-
    replace(T, Find, Replace, Result).

getRandomBoard(P):-
    getValidPositions(Positions),
    availablePiecesToPlace(Pieces),
    length(Pieces, L),
    random(0, L, Novo),
    translate(Novo, Val),
    write('Novo is: '),
    write(Val),

    P is Pieces.

translate(0, 'D'). %Black
translate(1, 'R'). %Red
translate(2, 'I'). %Ivory
translate(3, 'G'). %Green
translate(4, 'B'). %Blue
translate(5, 'W'). %WildColor