/* Game Draconis-26 by Julia Czosnek, Kinga Lukiewicz, Malgorzata Grzanka*/

/* Dynamic variables */
:- dynamic i_am_at/1, at/2, holding/1, alive/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

player_at(someplace).
at(thing, someplace).

/* These rules describe how to pick up an object. */
take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        player_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('Taken.'),
        !, nl.

take(_) :-
        write('It is not here.'),
        nl.

/* These rules describe how to go someplace. */

go(There) :-
    player_at(There),                 % check if already there
    !,
    write('You are already here.'), nl.

go(There) :-
    player_at(Here),
    retract(player_at(Here)),         % move player
    assert(player_at(There)),
    write('You move from '), write(Here),
    write(' to '), write(There), nl.

/* This rule tells how to look at your surroundings. */
look :-
        player_at(Place),
        describe(Place),
        nl.



/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

/* This is the description of rooms. */

/* This is the description of characters. */

/* These is it display the instruction. */
