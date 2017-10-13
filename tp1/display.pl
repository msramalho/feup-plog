/* This file implements the predicates required for displaying the board */

/*
Color  code     countInGame    countOffGame
Black:  D           8               1
Red:    R           8               1
Ivory:  I           8               1
Green:  G           8               1
Blue:   B           8               1
Wild:   W           3               0


Examples: delete afterwards
    0    1     2     3     4     5     6     7     8
 0                |     |     |     |     |
 1                |     |     |     |     |
 2                |     |     |     |     |     |
 3          |     |     |     |     |     |     |
 4          |     |     |     |     |     |     |
 5          |     |     |     |     |     |     |
 6    |     |     |     |     |     |     |     |     |
 7          |     |     |     |     |     |     |
 8          |     |     |     |     |     |     |
 9          |     |     |     |     |     |     |
10                |     |     |     |     |
11                |     |     |     |     |
12                |     |     |     |     |
-------------------------------------------------------
Player1 - R,B.
Player2 - I,G.
Now playing: Player1.


BRG - stack com G em baixo, R por cima de G, B por cima de

    0    1     2     3     4     5     6     7     8
 0          |     |  R  |     |     | GRB |
 1          |     |     |     |     |     |
 2          |     |     |     |     |     |     |
 3          |     |     |     |     |     |     |
 4          |     |     |     |     |     |     |
 5          |     |     |     |     |     |     |
 6    |     |     |     |DRBWI|     |     |     |     |
 7          |     |     |     |     |     |     |
 8          |     |     |     |     |     |     |
 9          |     |     |     |     |     |     |
10                |     |     |     |     |
11                |     |     |     |     |
12                |     |     |     |     |

*/