/* This file contains all the predicates required for the game logic to be implemented (allowed moves, ...)*/

/* Which board cells can be used (since it is not a board of squares) */

/*
validCells([
    [0, 0, 1, 0, 1, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 1, 0, 0],

    [0, 1, 0, 1, 0, 1, 0, 1, 0],
    [0, 1, 0, 1, 0, 1, 0, 1, 0],
    [0, 1, 0, 1, 0, 1, 0, 1, 0],

    [1, 0, 1, 0, 1, 0, 1, 0, 1],

    [0, 1, 0, 1, 0, 1, 0, 1, 0],
    [0, 1, 0, 1, 0, 1, 0, 1, 0],
    [0, 1, 0, 1, 0, 1, 0, 1, 0],

    [0, 0, 1, 0, 1, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 1, 0, 0]
]).
Mapping the valid cells like so into the predicate isValid(X,Y).
*/
/*[0, 0, 1, 0, 1, 0, 1, 0, 0]*/
/* first line of matrix */
isValid(0,2).
isValid(0,4).
isValid(0,6).
/* second line of matrix */
isValid(1,2).
isValid(1,4).
isValid(1,6).
/* third line of matrix */
isValid(2,2).
isValid(2,4).
isValid(2,6).

/*[0, 1, 0, 1, 0, 1, 0, 1, 0]*/
/* fourth line of matrix */
isValid(3,1).
isValid(3,3).
isValid(3,5).
isValid(3,7).
/* fifth line of matrix */
isValid(4,1).
isValid(4,3).
isValid(4,5).
isValid(4,7).
/* sixth line of matrix */
isValid(5,1).
isValid(5,3).
isValid(5,5).
isValid(5,7).

/*[1, 0, 1, 0, 1, 0, 1, 0, 1]*/
/* seventh line of matrix */
isValid(6, 0).
isValid(6, 2).
isValid(6, 4).
isValid(6, 6).
isValid(6, 8).

/*[0, 1, 0, 1, 0, 1, 0, 1, 0]*/
/* eigth line of matrix */
isValid(7,1).
isValid(7,3).
isValid(7,5).
isValid(7,7).
/* nineth line of matrix */
isValid(8,1).
isValid(8,3).
isValid(8,5).
isValid(8,7).
/* tenth line of matrix */
isValid(9,1).
isValid(9,3).
isValid(9,5).
isValid(9,7).

/*[0, 0, 1, 0, 1, 0, 1, 0, 0]*/
/* eleventh line of matrix */
isValid(10,2).
isValid(10,4).
isValid(10,6).
/* twelfth line of matrix */
isValid(11,2).
isValid(11,4).
isValid(11,6).
/* thirteenth line of matrix */
isValid(12,2).
isValid(12,4).
isValid(12,6).


/* convert the isValid predicate into a list of two lists containg the x and the y values of isValid*/
getValidPositions(Result):-
    findall(X, isValid(X,Y), L1),
    findall(Y, isValid(X,Y), L2),
    Result = [L1, L2].