/****************************************************************
 Game Draconis-26 by Julia Czosnek, Kinga Lukiewicz, Malgorzata Grzanka
 ****************************************************************/

/* Dynamic variables */
:- dynamic player_at/1, at/2, holding/1, alive/1.
:- dynamic hints_counter/1, chosen/1.
:- dynamic lights_on/0.
:- dynamic force_investigation/0.
:- dynamic investigated/1.
:- dynamic shuttle_closed/0.
:- dynamic investigated/1.
:- dynamic investigated_quarters/0.
:- dynamic put_used/0.
:- dynamic noises_heard/0.
:- dynamic grab_used/0.
:- dynamic countdown/1.

/* These rules define rooms existence. */
room(technical_room).
room(living_quarters).
room(medbay).
room(storage_bay).
room(power_room).
room(shuttle).

/* These rules define characters. */
character(fluff).
character(dallas).
character(becker).
character(lambert).
character(reed).
character(walker).

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

% check if room is shuttle and closed
go(shuttle) :-
    shuttle_closed,
    !,
    write('Cannot enter the shuttle. Only available in case of code red.'),
    nl.

% check if the room exists
go(There) :-
    \+ room(There),                 
    !,
    write('There is no such room as '),
    write(There), nl,
    write('To see available rooms type \'rooms.\''),
    nl.

% check if MU/TH/ER waits for your choice
go(_) :-
    \+ put_used,
    !,
    write('MU/TH/ER is waiting for your decision. You can\'t leave yet.'),
    nl.

% check if already in the room
go(There) :-
    player_at(There),
    !,
    write('You are already here.'), nl.

% if force_investigation is true
go(_) :-
    force_investigation,
    !,
    write('\'Where the hell are you going, Ripley?\' Reed snaps, grabbing your arm and pulling you back into the room. \'You\'re not leaving until we figure out what happened here. You\'re the warrant officer - you lead the investigation.\''), nl,
    nl.

go(There) :-
    player_at(Here),
    retract(player_at(Here)),          % move player
    assert(player_at(There)),

    % Print movement message once
    ( There == medbay ->
        write('You enter the medbay, noticing the isolation space.'), nl
    ;   write('You enter the '), write(There), nl
    ),

    % Handle sabotour
    (There == power_room, noises_heard ->
        confrontation
    ;
        noises_heard ->
        ignoring_noises
    ;
        true
    ),

    % Handle power room noises
    (There == medbay, \+ noises_heard, lights_on ->
        second_body
    ;
        There \= power_room, \+ noises_heard, lights_on ->
        noise_power_room
    ;
        true
    ),

    % Handle lights
    ( There == living_quarters, \+ lights_on ->
        first_body
    ;
        \+ lights_on ->
        write('You almost trip over something you can’t see. Everything is still pitch black.'), nl
    ;
        true
    ),

    nl.

/* This rule tells how to look at your surroundings. */
/* IT CANNOT WORK WHEN LIGHTS TURNED OFF */
look :-
    player_at(Place),
    (
        \+ lights_on ->
            write('Everything is still pitch black. You can\'t see anything.'), nl
        ;
            describe(Place, Text),
            write(Text), nl
    ),
    nl.

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(medbay, Text) :-
    ( holding(fluff) ->
        Text = 'Becker’s body lies torn on the floor, blood pooling in jagged, dark patterns. The black substance coats the tiles, thicker than in Dallas’s murder. Fluff mews softly in your arms, nudging you as if urging you to leave this place.'
    ;
        Text = 'Becker’s body is torn apart on the floor, blood pooling in dark, jagged patterns. The black substance spreads across the tiles, darker and thicker than in Dallas’s murder. A faint meowing echoes from a cupboard.'
    ).

describe(living_quarters, Text) :-
    ( \+ investigated_quarters ->
        assert(investigated_quarters),
        Text = 'The blood is still spreading across the floor. A strange black substance mixes with it in the wounds. They don’t look human-made - jagged and mangled, as if something with a jaw full of sharp teeth tore them.'
    ;
        Text = 'The beds are neatly made, the desks empty, and everything seems in order - except for the body lying in the middle of the room.'
    ).

describe(technical_room, 'The servers hum steadily. MU/TH/ER’s screen glows softly, waiting silently for your next command').

describe(storage_bay, 'Rows of shelves line the room, scattered with guns catching the dim light, silent and waiting.').

describe(power_room, 'The power room hums with machinery. Flickering panels cast shifting shadows, and the air smells faintly of burnt metal.').

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
    write('Fluff      -- spaceship\'s cat.'), nl,
    write('Dallas     -- captain of the Nostromo spaceship.'), nl,
    write('Lambert    -- navigator.'), nl,
    write('Walker     -- chief engineer.'), nl,
    write('Becker     -- executive officer.'), nl,
    write('Reed       -- science officer.'), nl,
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
    write('MU/TH/ER, main spaceship\s computer hums softly when it prints the response on the screen.'), nl,
    write('MU/TH/ER: Hello, Warrant Officer Ripley. Here is the report you requested.'), nl,
    write('Report of Mission 067801'), nl,
    write('Time 9036727h: Corporate command authorizes the spaceship Nostromo to investigate a possible life form on planet 26-Draconis.'), nl,
    write('Time 9036911h: Nostromo lands on the surface of 26-Draconis. Executive Officer Becker and Science Officer Reed leave the ship to investigate.'), nl,
    write('Time 9036916h: Nostromo loses contact with Executive Officer Becker. Science Officer Reed reports unsuccessful search attempts.'), nl,
    write('MU/TH/ER: Anything else I can do for you, Officer?'), nl, nl,
    write('Before you can respond, the main console clears. A new line appears.'), nl,
    write('MU/TH/ER: Science Officer Reed has re-entered the Nostromo carrying the sick and unconcious Executive Officer Becker. His spacesuit is breached. Per quarantine law, the crew must be contained. Should I send the command to move him to the medbay for treatment, or to isolation to prevent potential contamination?'), nl,
    write('Type either \'put(medbay).\' or \'put(isolation).\''), nl,
    nl.

/* These rules handle put logic. */
valid_put(medbay).
valid_put(isolation).

put(X) :-
    \+ valid_put(X),
    !,
    write('You must choose medbay or isolation.'), nl,
    write('Try again.'), nl,
    true.

put(_) :-
    put_used,
    !,
    write('You already made your choice. It is too late to change it now.'),
    nl.

put(X) :-
    retractall(chosen(_)),
    assert(chosen(X)),
    handle_choice(X),
    assert(put_used),
    power_off_scene.

handle_choice(isolation) :-
    retract(hints_counter(N)),
    N1 is N + 1,
    assert(hints_counter(N1)). 

handle_choice(_) :-
    true.

/* This rule describes power off scene. */
power_off_scene :-
    retractall(lights_on),
    write('MU/TH/ER: ... Command sent.'), nl,
    write('Per Corporate protocol, every minor decision must be logged, so you update the mission report. You stretch and rise from the console, planning to look for the ship’s cat, Fluff.'), nl,
    write('As you step into the corridor, the lights go out. The ship is plunged into darkness. The only sound is your own heartbeat, pounding in your ears. Your breath catches when you hear a scream - and stops entirely when it’s cut short.'), nl,
    write('You remember the emergency procedure: in a total blackout, all crew members are to gather in the living quarters.'), nl,
    nl.

/* This rule decribes finding the first body scene. */

first_body :-
    assert(lights_on),
    assert(force_investigation),
    retract(alive(dallas)),
    assert(at(dallas, living_quarters)),
    assert(at(lambert, living_quarters)),
    assert(at(reed, living_quarters)),
    write('You step into the living quarters. The lights flicker back on, blinding you for a moment.'), nl,
    write('A scream cuts through the silence - Lambert\'s. As your eyes adjust, you see it: a body sprawled in the middle of the room, torn open, blood spreading across the floor like a shadow. It\'s Dallas - your captain and friend.'), nl,
    write('Only Lambert and Reed are here. With the captain dead and Becker still unconscious, the only one unaccounted for is Walker, the chief engineer - he must\'ve restored the power.'), nl,
    write('Reed turns to you, his voice tight. \'You\'re the one in command now, Ripley. What do we do?\''), nl,
    write('Do you look around the room first, or investigate one of the crew members?'),
    nl.

/* These rules describe investigations.*/

% does person exists
investigate(Person) :-
    \+ character(Person),
    !,
    write('There is no such character as '),
    write(Person), nl,
    write('To see characters type \'characters.\''), nl.

% is person in the room
investigate(Person) :-
    player_at(Place),
    \+ at(Person, Place),
    !,
    write('You can\'t investigate them - they\'re not in the room with you.'), nl,
    nl.

% fluff investigation case
investigate(fluff) :-
    !,
    write('\'What can you tell me, Fluff?\''), nl,
    write('The cat looks at you with his big green eyes curiously and only softly meows back.'), nl,
    nl.

% if person is dead
investigate(Person) :-
    \+ alive(Person),
    !,
    write('There\'s no point investigating the dead.'), nl,
    nl.

% if person was already investigated
investigate(Person) :-
    investigated(Person),
    !,
    write('\'I have nothing more to say.\' is all they say.'), nl,
    nl.

% proper investigation
investigate(Person) :-
    assert(investigated(Person)),
    ( Person == lambert ->
        retract(force_investigation),
        write('\'You were the first here, right, Lambert?\''), nl,
        write('\'Yeah,\' she says, voice trembling. \'I was walking down the corridor when the power went out. Then I heard the scream and...\' She glances tearfully at Dallas’ body. \'I don’t know who or what could have done this.\''), nl,
        write('\'Did you notice anything else?\''), nl,
        write('\'Not much,\' she says, shivering. \'Except for Fluff. The cat ran between my legs just before the scream. He was clearly rattled, yowling and howling.\''), nl,
        write('\'Fluff’s instincts were always sharp,\' you think to yourself. \'If he sensed danger before anyone else... maybe he’s seen what we haven’t.\''), nl,
        walker_joins

    ; Person == reed ->
        write('\'What were you doing just now, Reed?\''), nl,
        write('\'I was running an analysis of Becker’s blood,\' he says. \'Trying to figure out how to help him... his condition isn’t improving.\''), nl,
        write('\'Did you find anything?\''), nl,
        write('\'Not exactly,\' Reed admits. \'But something’s wrong. His blood is more acidic than normal, and it’s not clotting at all.\''), nl,
        nl
    
    ; Person = walker ->
        write('\'What do you mean by two people, Walker?\''), nl,
        ( chosen(medbay) ->
            write('\'I was fixing the power after it went out,\' he says. \'Then I wanted to go straight to our quarters, but the medbay door was open. There was blood everywhere - Becker\'s blood, I think. I didn\'t want to investigate without you.\''),
            nl
        ; chosen(isolation) ->
            retract(hints_counter(N)),
            N1 is N + 1,
            assert(hints_counter(N1)),
            write('\'I was fixing the power after it went out,\' he says. \'Then I wanted to go straight to our quarters, but the medbay door was open and there was blood everywhere. So I went to check the isolation, and...\''), nl,
            write('\'Did Becker leave quarantine?\''), nl,
            write('\'Not exactly,\' Walker replies. \'He’s still in isolation - but he\'s dead, Ripley. Blood everywhere, his body torn apart. The strange thing is, no alarm went off, so it wasn\'t a malfunction. Someone on the crew must have unlocked the door.\''), nl
        ; true
        ),
        nl

    ; true
    ).

/* This rule described the scene where chief engineer barges in*/

walker_joins :-
    assert(at(walker, living_quarters)),
    write('Before you can decide what to do next,  Walker - chief engineer - bursts in.'), nl,
    write('\'I fixed the po-\' he stops, startled by Dallas\'s dead body. \'What the hell happened here?\''), nl,
    write('\'The captain\'s dead,\' Reed says. \'Where have you been?\''), nl,
    write('\'When the lights went out, I went to the power room to restore them,\' Walker explains. \'I didn’t expect two people to die while I was gone.\''), nl,
    write('Wait, did he just say two?'), nl,
    nl.

/* These rules describe the scenes where we hear the noises from power room */
second_body :-
    retract(hints_counter(N)),
    N1 is N + 1,
    assert(hints_counter(N1)),
    assert(at(reed, medbay)),
    write('Becker\'s body lies scattered across the floor, blood seeping into jagged patterns. It looks as though something forced its way out of him - ripping through his chest from the inside. The black substance from before slicks every surface, thicker now, spreading across the tiles like living oil.'), nl,
    write('A faint meow breaks the silence. Fluff peers out from a cupboard, fur bristling, eyes locked on the floor as if urging you to notice something. You follow his gaze and spot a discarded multitool beside the cupboard.'), nl,
    write('A sudden scream echoes from the power room, followed by a harsh mechanical noise. Your breath catches.'), nl,
    write('The door swings open. Reed steps inside, pale and grim.'), nl,
    write('\'And then there were two,\' he whispers. \'There\'s one more body to find... and the killer.\''), nl,
    write('\'How do I know you\'re not the killer?\''), nl,
    write('\'You don\'t,\' he admits. \'But I can go with you to investigate - or you can go alone.\''), nl,
    write('If you want to take Reed with you, type \'grab(reed).\' before going to the next room.'), nl,
    (   N1 >= 2 ->
        write('At this point, you\'re certain someone on the crew is working with the alien. It could be Reed - but if it were, why hasn\'t he killed you yet?'), nl,
        write('Better not to split up when there might be another enemy aboard.'), nl
    ;   true
    ),
    assert(noises_heard),
    nl.

noise_power_room :-
    write('A strange noise comes from the power room, followed by a scream. Your breath catches.'), nl,
    write('The door swings open. Reed steps inside, pale and grim.'), nl,
    write('\'And then there were two,\' he whispers. \'There\'s one more body to find... and the killer.\''), nl,
    write('\'How do I know you\'re not the killer?\''), nl,
    write('\'You don\'t,\' he admits. \'But I can go with you to investigate - or you can go alone.\''), nl,
    write('If you want to take Reed with you, type \'grab(reed).\' before going to the next room.'), nl,
    retract(hints_counter(N)),
    N1 is N + 1,
    assert(hints_counter(N1)),
    (   N1 >= 2 ->
        write('At this point, you\'re certain someone on the crew is working with the alien. It could be Reed - but if it were, why hasn\'t he killed you yet?'), nl,
        write('Better not to split up when there might be another enemy aboard.'), nl
    ;   true
    ),
    assert(noises_heard),
    nl.
	
/* This rule tells how to grab Reed. */
valid_grab(reed).
grab(X) :-
    \+ valid_grab(X),
    !,
    write('You can only take Reed with you. There is no one else here.'), nl,
    write('Try again.'), nl,
    true.

grab(_) :-
    grab_used,
    !,
    write('Reed is already with you.'),
    nl.

grab(_) :-
    assert(grab_used),
    write('Reed will go with you.').

/* These rules describe saboteur scenes */
confrontation :-
    countdown(_), !.

confrontation :-
    (grab_used -> 
        write('DA/TU/ER, the secondary computer, hums softly - a corporate file open.'), nl,
        write('DA/TU/ER: Access granted. Update on mission 067801: Corporate directive changed. Priority one: Ensure return of the organism for analysis. Crew expendable.'), nl,
        write('Cold hands clamp around your throat - it\'s Walker.'), nl,
        write('Reed swings a metal pipe, but Walker catches him mid-strike and slams him into the console. A sickening crack echoes as Reed\'s body crumples to the floor, his neck bent at an unnatural angle.'), nl,
        write('You kick Walker back into DA/TU/ER; sparks explode as his head smashes through the screen, wires spilling from the wound instead of blood'), nl,
        write('\'You\'re... an android?\' you gasp.'), nl,
        write('\'The organism must survive,\' he rasps, voice glitching. \'It\'s evolving... indestructable now.\''), nl,
        write('He convulses violently, circuits flaring, and DA/TU/ER\'s lights turn red.'), nl,
        write('You hear MU/TH/ER\'s automated voice through the speakers:'), nl,
        write('\'Code red. Auto-destruction sequence initiated.'), nl,
        write('Completion in three minutes. All crew members proceed to the shuttle immediently.\''), nl,
        write('Somewhere in the ship, you think you hear Fluff\'s distant yowl - a reminder that not everything worth saving here is human.'), nl,
        retract(alive(reed)),
        retract(alive(walker)),
        retract(shuttle_closed),
        retractall(countdown(_)),
        assert(countdown(3)),
        nl
    ;
        write('It\'s empty - nobody in sight.'), nl,
        write('A second computer, DA/TU/ER hums softly. Its screen displays a corporate history log. You glance through it.'), nl,
        write('DA/TU/ER: Access granted. Update on mission 067801: Corporate directive changed. Priority one: Ensure return of the organism for analysis. All other considerations secondary. Crew expendable.'), nl,
        write('Before you can process the message, a gun presses against the back of your head. The safety clicks, a loud BANG echoes - and everything goes black.'), nl,
        write('DA/TU/ER: Updated report for mission 067801: Time 9036919h: Warrant Officer Ripley found dead.'), nl,
        retract(alive(player)),
        stop
    ).

ignoring_noises :-
    countdown(_), !.

ignoring_noises :-
    write('Before you can do that, the spaceship alarm goes off. You hear MU/TH/ER automated voice through the speakers:'), nl,
    write('\'Code red. Auto-destruction sequence initiated. Completion in three minutes. All crew members proceed to the shuttle immediently.\''), nl,
    retract(shuttle_closed),
    retractall(countdown(_)),
    assert(countdown(3)),
    nl.

% THE STORY MUST BE CONTINUED FROM HERE

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
    retractall(hints_counter(_)),
    retractall(chosen(_)),
    retractall(investigated(_)),

    % set initial values
    assert(player_at(technical_room)),
    assert(alive(player)),
    assert(alive(fluff)),
    assert(alive(lambert)),
    assert(alive(dallas)),
    assert(alive(walker)),
    assert(alive(reed)),
    assert(alive(becker)),
    assert(hints_counter(0)),
    assert(lights_on),
    assert(shuttle_closed),

    % Put stuff & people in places
    assert(at(fluff, medbay)),
    assert(at(gun, storage_bay)),
    assert(at(multitool, medbay)),

    write('Do you want to play a game?'), nl,
    write('You are the Warrant Officer aboard the spaceship Nostromo, on a mission to investigate a newly discovered life form. But something  has gone horribly wrong - and the alien creature may not be the only danger lurking in the ship’s dark corridors...'), nl,
    write('But before you continue your journey:'),
    instructions,
    nl.
