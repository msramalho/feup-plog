/*
claim.pl: predicates related to the color claiming operation
*/
updateClaimedColor(Translated):-
    toClaim(ToClaim), %load the colors left for claiming
    nth0(_, ToClaim, Translated).%if the chosen color is inside ToClaim
updateClaimedColor(_):-%if update fails then it's because:
    write('This color is no longer available\n'), fail.

readValidColor(Translated):-
    write('Which color would you like to claim?\n'),
    read_line(Color),
    translateColor(Color, Translated), !,
    updateClaimedColor(Translated).
readValidColor(_):-%if readValid fails it's because:
    write('Invalid color name.\n'), fail.

%fails if the user cannot claim
validClaim:-
    getColors(ChosenColors), %get the current player player's colors
    length(ChosenColors, Len), %get the length of this player's colors
    Len < 2. %if the length is less than 2, then read color else go to other option for claimColor

%changes the game state by claiming a color
claim(Color):-
    getColors(ChosenColors), %get the current player player's colors
    append([ChosenColors, [Color]], Result),
    saveGetColors(Result),
    saveHasClaimed(true),%set the flag hasClaimed to true
    toClaim(ToClaim),
    nth0(_, ToClaim, Color, NewToClaim),%if the chosen color is inside ToClaim
    saveToClaim(NewToClaim). %updated available colors

claimColor:-
    hasClaimed(true), write('You can only claim one color per turn\n'), !, fail.

claimColor:-
    validClaim, !,
    repeat,%repeatedly read color
        readValidColor(Translated),
    !,
    claim(Translated), fail.%,write('new colors for '), write(CurrentPlayer), write(' are: '), write(Result), nl.

claimColor:-
    write('You can only claim two colors\n').



%colors transaltion
translateColor("black", black).
translateColor("D", black).
translateColor("d", black).

translateColor("red", red).
translateColor("R", red).
translateColor("r", red).

translateColor("ivory", ivory).
translateColor("I", ivory).
translateColor("i", ivory).

translateColor("green", green).
translateColor("G", green).
translateColor("g", green).

translateColor("blue", blue).
translateColor("B", blue).
translateColor("b", blue).

% fails if this color is not a claimable one
isClaimableColor(Color):-
    toClaim(Colors),
    nth0(_, Colors, Color).
%from the initial colors, which can be claimed
claimableColors([black, red, ivory, green, blue]).