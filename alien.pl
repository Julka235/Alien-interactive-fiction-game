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

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

/* This rule displays 'GAME OVER' message for the player. */
game_over:-
    nl,
    write('GAME OVER. Hope you join the Nostromo again soon..'), nl,
    

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

/* This rule prints the description of the rooms. */

rooms :-
    nl,
    write('Here is the list of rooms aboard the Nostromo spaceship.'), nl,
    write('living_quarters      -- where the crew sleeps and eats.'), nl,
    write('medbay               -- medical bay with an internal isolation space.'), nl,
    write('storage_bay          -- storage for weapons and canned supplies.'), nl,
    write('technical_room       -- houes the main computer, MU/TH/ER.'), nl,
    write('power_room           -- controls the ship\'s entire power system.'), nl,
    write('shuttle              -- escape vessel for emergency departure.'), nl,
    nl.

/* This rule prints the description of the crew. */
crew :-
    nl,
    write('Here is the list of the members of your crew. If you want to interact with them - use their name in lowercase.'), nl,
    write('Fluff     -- spaceship\'s cat.'), nl,
    write('Dallas    -- captain of the Nostromo spaceship.'), nl,
    write('Lambert   -- navigator.'), nl,
    write('Walker    -- chief engineer.'), nl,
    write('Becker    -- executive officer.'), nl,
    write('Reed      -- science officer.'), nl,
    nl.


/* This rule displays the instruction. */
instructions :-
    nl,
    write('Here is the list of available commands:'), nl,
    write('start.               -- to start the game.'), nl,
    write('go(Room).            -- to enter the room.'), nl,
    write('take(Object).        -- to pick up an object.'), nl,
    write('look.                -- to look around the room.'), nl,
    write('investigate(Person). -- to investigate someone.'), nl,
    write('crew.                -- to see the list of the crew.'), nl,
    write('rooms.               -- to see the list of the rooms.'), nl,
    write('instructions.        -- to see this list again.'), nl,
    write('halt.                -- to end the game and quit.'), nl,
    nl.

/* This rule starts the game. */

/****** GAME PLOT PART ***************************************************/
