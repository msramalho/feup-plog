
updateClaimedColor(Translated):-
    toClaim(ToClaim), %load the colors left for claiming
    nth0(_, ToClaim, Translated, NewToClaim),%if the chosen color is inside ToClaim
    retract(toClaim(_)),
    assert(toClaim(NewToClaim)). %updated available colors
updateClaimedColor(_):-%if update fails then it's because:
    write('This color is no longer available\n'), fail.

readValidColor(Translated):-
    write('Which color would you like to claim?\n'),
    read_line(Color),
    translateColor(Color, Translated),
    updateClaimedColor(Translated).
readValidColor(_):-%if readValid fails it's because:
    write('Invalid color name.\n'), fail.

claimColor:-
    player(CurrentPlayer),%load the current player
    getColors(CurrentPlayer, ChosenColors), %get this player's colors
    length(ChosenColors, Len), %get the length of this player's colors
    Len < 2, !, %if the length is less than 2, then read color else go to other option for claimColor
    repeat,%repeatedly read color
        readValidColor(Translated),
    !,
    append([ChosenColors, [Translated]], Result),
    write('new colors for '), write(CurrentPlayer), write(' are: '), write(Result),nl,
    retract(getColors(CurrentPlayer, _)),
    assert(getColors(CurrentPlayer, Result)),
    retract(hasClaimed(false)),
    assert(hasClaimed(true)).%set the flag hasClaimed to true

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



claimableColors([black, red, ivory, green, blue]).%this will be redacted and asserted