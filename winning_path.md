# One of the possible paths to win the game
## WARNING: Spoilers ahead
Do you want to play a game?
You are the Warrant Officer aboard the spaceship Nostromo, on a mission to investigate a newly discovered life form. But something  has gone horribly wrong - and the alien creature may not be the only danger lurking in the ship’s dark corridors...
But before you continue your journey:
Here is the list of available commands:
start.               -- to start the game.
go(Room).            -- to enter the room.
take(Object).        -- to pick up an object.
look.                -- to look around the room.
investigate(Person). -- to investigate someone.
crew.                -- to see the list of the crew.
rooms.               -- to see the list of the rooms.
instructions.        -- to see this list again.
stop.                -- to end the game.

?- start.
MU/TH/ER, main spaceship's computer hums softly when it prints the response on the screen.
MU/TH/ER: Hello, Warrant Officer Ripley. Here is the report you requested.
Report of Mission 067801
Time 9036727h: Corporate command authorizes the spaceship Nostromo to investigate a possible life form on planet 26-Draconis.
Time 9036911h: Nostromo lands on the surface of 26-Draconis. Executive Officer Becker and Science Officer Reed leave the ship to investigate.
Time 9036916h: Nostromo loses contact with Executive Officer Becker. Science Officer Reed reports unsuccessful search attempts.
MU/TH/ER: Anything else I can do for you, Officer?

Before you can respond, the main console clears. A new line appears.
MU/TH/ER: Science Officer Reed has re-entered the Nostromo carrying the sick and unconcious Executive Officer Becker. His spacesuit is breached. Per quarantine law, the crew must be contained. Should I send the command to move him to the medbay for treatment, or to isolation to prevent potential contamination?
Type either 'put(medbay).' or 'put(isolation).'

true.

?- put(isolation).
MU/TH/ER: ... Command sent.
Per Corporate protocol, every minor decision must be logged, so you update the mission report. You stretch and rise from the console, planning to look for the ship’s cat, Fluff.
As you step into the corridor, the lights go out. The ship is plunged into darkness. The only sound is your own heartbeat, pounding in your ears. Your breath catches when you hear a scream - and stops entirely when it’s cut short.
You remember the emergency procedure: in a total blackout, all crew members are to gather in the living quarters.

true.

?- go(living_quarters).
You enter the living_quarters
You step into the living quarters. The lights flicker back on, blinding you for a moment.
A scream cuts through the silence - Lambert's. As your eyes adjust, you see it: a body sprawled in the middle of the room, torn open, blood spreading across the floor like a shadow. It's Dallas - your captain and friend.
Only Lambert and Reed are here. With the captain dead and Becker still unconscious, the only one unaccounted for is Walker, the chief engineer - he must've restored the power.
Reed turns to you, his voice tight. 'You're the one in command now, Ripley. What do we do?'
Do you look around the room first, or investigate one of the crew members?

true.

?- look.
The blood is still spreading across the floor. A strange black substance mixes with it in the wounds. They don’t look human-made - jagged and mangled, as if something with a jaw full of sharp teeth tore them.

true.

?- investigate(lambert).
'You were the first here, right, Lambert?'
'Yeah,' she says, voice trembling. 'I was walking down the corridor when the power went out. Then I heard the scream and...' She glances tearfully at Dallas’ body. 'I don’t know who or what could have done this.'
'Did you notice anything else?'
'Not much,' she says, shivering. 'Except for Fluff. The cat ran between my legs just before the scream. He was clearly rattled, yowling and howling.'
'Fluff’s instincts were always sharp,' you think to yourself. 'If he sensed danger before anyone else... maybe he’s seen what we haven’t.'
Before you can decide what to do next,  Walker - chief engineer - bursts in.
'I fixed the po-' he stops, startled by Dallas's dead body. 'What the hell happened here?'
'The captain's dead,' Reed says. 'Where have you been?'
'When the lights went out, I went to the power room to restore them,' Walker explains. 'I didn’t expect two people to die while I was gone.'
Wait, did he just say two?

true.

?- investigate(walker).
'What do you mean by two people, Walker?'
'I was fixing the power after it went out,' he says. 'Then I wanted to go straight to our quarters, but the medbay door was open and there was blood everywhere. So I went to check the isolation, and...'
'Did Becker leave quarantine?'
'Not exactly,' Walker replies. 'He’s still in isolation - but he's dead, Ripley. Blood everywhere, his body torn apart. The strange thing is, no alarm went off, so it wasn't a malfunction. Someone on the crew must have unlocked the door.'

true.

?- go(medbay).
You enter the medbay, noticing the isolation space.
Becker's body lies scattered across the floor, blood seeping into jagged patterns. It looks as though something forced its way out of him - ripping through his chest from the inside. The black substance from before slicks every surface, thicker now, spreading across the tiles like living oil.
A faint meow breaks the silence. Fluff peers out from a cupboard, fur bristling, eyes locked on the floor as if urging you to notice something. You follow his gaze and spot a discarded multitool beside the cupboard.
A sudden scream echoes from the power room, followed by a harsh mechanical noise. Your breath catches.
The door swings open. Reed steps inside, pale and grim.
'And then there were two,' he whispers. 'There's one more body to find... and the killer.'
'How do I know you're not the killer?'
'You don't,' he admits. 'But I can go with you to investigate - or you can go alone.'
If you want to take Reed with you, type 'grab(reed).' before going to the next room.
At this point, you're certain someone on the crew is working with the alien. It could be Reed - but if it were, why hasn't he killed you yet?
Better not to split up when there might be another enemy aboard.

true.

?- take(fluff).
Taken.
true.

?- grab(reed).
Reed will go with you.
true.

?- go(power_room).
You enter the power_room
DA/TU/ER, the secondary computer, hums softly - a corporate file open.
DA/TU/ER: Access granted. Update on mission 067801: Corporate directive changed. Priority one: Ensure return of the organism for analysis. Crew expendable.
Cold hands clamp around your throat - it's Walker.
Reed swings a metal pipe, but Walker catches him mid-strike and slams him into the console. A sickening crack echoes as Reed's body crumples to the floor, his neck bent at an unnatural angle.
You kick Walker back into DA/TU/ER; sparks explode as his head smashes through the screen, wires spilling from the wound instead of blood
'You're... an android?' you gasp.
'The organism must survive,' he rasps, voice glitching. 'It's evolving... indestructable now.'
He convulses violently, circuits flaring, and DA/TU/ER's lights turn red.
You hear MU/TH/ER's automated voice through the speakers:
'Code red. Auto-destruction sequence initiated.
Completion in three minutes. All crew members proceed to the shuttle immediently.'
Somewhere in the ship, you think you hear Fluff's distant yowl - a reminder that not everything worth saving here is human.
You have only three minutes to get off this ship... That means you can visit up to three rooms, including the shuttle. Grab what you need quickly and make your way to the shuttle!


true.

?- go(storage_bay).
You can visit 2 more rooms - with the shuttle bay as the last one - before the autodestruction sequence begins.

You enter the storage_bay

true.

?- look.
Rows of shelves line the room, scattered with guns catching the dim light, silent and waiting for you to grab one.

true.

?- take(gun).
Taken.
true.

?- go(shuttle).
You reach the shuttle just in time and take off.
You update the mission report, then prepare for stasis. Exhausted, you climb into the capsule.
Fluff hisses at the food cupboard. Your heart race - you realize the alien has made it abroad.
Quickly, you grab a spacesuit, put it on, and hide Fluff inside with you. You secure yourself to the navigator's seat, keep your gun ready, and open the airlock.
When the alien lunges, you fire, blasting it into space.
Finally, you disarm yourself and settle into stasis, Fluff safe by your side.

Congrats! You and Fluff have survived the alien attack Hope to see you on the next mission, Officer.