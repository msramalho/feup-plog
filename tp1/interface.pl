wulc:-put_code(9484). % write upper left corner
wurc:-put_code(9488). % write upper right corner
wllc:-put_code(9492). % write lower left corner
wlrc:-put_code(9496). % write lower right corner

wvd:-put_code(9474). % write vertical dash
%write dash
wd:-put_code(9472).
wd(0).
wd(N):-wd, N1 is N-1, wd(N1).

wel(Len):-wel(Len, ' ').
wel(Len, Content):-
	Start = '~|~t~s~t~', End = '+',
	atom_concat(Start, Len, S1), atom_concat(S1, End, S2),
	wvd, format(S2, Content), wvd, nl.

displayMenu:-
	write('\33\[2J'),
	wulc, wd(40), wurc, nl,
	wel('30'), wel('30'), wel('30'),
	wel('30', 'Welcome to'),
	wel('30', 'LYNGK'),
	wel('30'), wel('30'),wel('30'),wel('30'),
	wllc, wd(40), wlrc.

