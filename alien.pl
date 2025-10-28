/****************************************************************
 Game Draconis-26 by Julia Czosnek, Kinga Lukiewicz, Malgorzata Grzanka
 ****************************************************************/

/* Dynamic variables */
:- dynamic player_at/1, at/2, holding/1, alive/1.
:- dynamic counter/1, chosen/1, player_at/1.

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
    write('stop.                -- to end the game.'), nl,
    nl.

/* This rule displays 'GAME OVER' message for the player. */
stop :-
    nl,
    write('GAME OVER. Hope you join the Nostromo again soon.'), nl,
    halt,
    write(' Please enter the "halt." command to close console.'), nl,
    nl.
/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */


/****** GAME PLOT PART ***************************************************/

/* This rule starts the game. */
start :-
    nl,
    write('MU/TH/ER: Hello, Warrant Officer Ripley. Here is the report you requested.'), nl,
    write('Report of Mission 067801'), nl,
    write('Time 9036727h: Corporate command authorizes the spaceship Nostromo to investigate a possible life form on planet 26-Draconis.'),
    write('Time 9036911h: Nostromo lands on the surface of 26-Draconis. Executive Officer Becker and Science Officer Reed leave the ship to investigate.'),
    write('Time 9036916h: Nostromo loses contact with Executive Officer Becker. Science Officer  Reed reports unsuccessful search attempts.'), nl,
    write('MU/TH/ER: Anything else I can do for you, Officer?'), nl, nl,
    write('Before you can respond, the main console clears. A new line appears.'), nl,
    write('MU/TH/ER: Science Officer Reed has re-entered the Nostromo carrying the sick and unconcious Executive Officer Becker. His spacesuit is breached. Per quarantine law, the crew must be contained. Should I send the command to move him to the medbay for treatment, or to isolation to prevent potential contamination?'), nl,
    write('Type either \'put(medbay).\' or \'put(isolation).\''), nl,
    nl.

/* These rules handle choice mechanism. */
valid_choice(medbay).
valid_choice(isolation).

put(X) :-
    valid_choice(X),
    retractall(chosen(_)),
    assert(chosen(X)),
    handle_choice(X),
    power_off_scene.

put(X) :-
    \+ valid_choice(X),        % if choice is invalid
    write('You must choose medbay or isolation.'), nl,
    write('Try again.'), nl,
    true.

handle_choice(isolation) :-
    assert(counter(1)).

handle_choice(_) :-
    true.

/* This rule describes next scene - when power goes down. */
power_off_scene :-
    write('MU/TH/ER: ... Command sent.'), nl,
    write('Per Corporate protocol, every minor decision must be logged, so you update the mission report. You stretch and rise from the console, planning to look for the ship’s cat, Fluff.'), nl,
    write('As you step into the corridor, the lights go out. The ship is plunged into darkness. The only sound is your own heartbeat, pounding in your ears. Your breath catches when you hear a scream - and stops entirely when it’s cut short.'), nl,
    write('You remember the emergency procedure: in a total blackout, all crew members are to gather in the living quarters.'), nl,
    nl.

% next move

/****** MAIN *************************************************************/
/* This rule is displayed after executing this file.
Per Prolog guidelines initialization best be at the bottom of the file. */
:- initialization(main).

main :-
    % clear old data
    retractall(player_at(_)),
    retractall(at(_, _)),
    retractall(holding(_)),
    retractall(alive(_)),
    retractall(counter(_)),
    retractall(chosen(_)),

    % set initial values
    assert(player_at(technical_room)),
    assert(at(thing, someplace)), % TBD - update initializations
    assert(alive(player)), % TBD - update characters to be alive
    assert(counter(0)),
    write('Do you want to play a game?'), nl,
    write('You are the Warrant Officer aboard the spaceship Nostromo, on a mission to investigate a newly discovered life form. But something  has gone horribly wrong - and the alien creature may not be the only danger lurking in the ship’s dark corridors...'), nl,
    write('But before you continue your journey:'),
    instructions,
    nl.
