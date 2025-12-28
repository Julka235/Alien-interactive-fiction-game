 "Alien text Game
Małgorzata Grzanka
Julia Czosnek
Kinga Łukiewicz
Politechnika Warszawska 2025"


Object subclass: Room [
    | name description helpDescription isOpen investigated |

    Room class >> name: aName isOpen: openFlag description: aDesc helpDescription: aHelpDesc [
        ^super new initName: aName isOpen: openFlag description: aDesc helpDescription: aHelpDesc
    ]

    initName: aName isOpen: openFlag description: aDesc helpDescription: aHelpDesc [
        name := aName.
        description := aDesc.
        helpDescription := aHelpDesc.
        isOpen := openFlag.
        investigated := false.
        ^ self
    ]

    getName [
        ^ name
    ]

    canPlayerEnter [
        ^ isOpen
    ]
    describe [
        ^ helpDescription
    ]

    look [
        (name = 'LivingQuarters' and: [investigated not])
            ifTrue: [
                Transcript show: 'The blood is still spreading across the floor. A strange black substance mixes with it in the wounds. They don’t look human-made — jagged and mangled, as if something with a jaw full of sharp teeth tore them.'; cr.
                investigated := true.
            ]
            ifFalse: [
                Transcript show: description; cr.
            ].
    ]
]

Object subclass: CrewMember [
    | name location isAlive canBeGrabbed isGrabbed investigated investigationLine description |

    CrewMember class >> name: aName location: aRoom canBeGrabbed: grabFlag description: aDescription [
        ^super new initName: aName location: aRoom canBeGrabbed: grabFlag description: aDescription
    ]

    CrewMember >> isAlive: aBoolean [
        isAlive := aBoolean.
    ]

    CrewMember >> investigationLine: aString [
    investigationLine := aString.
    ]

    CrewMember >> location: aRoom [
    location := aRoom.
    ]

    initName: aName location: aRoom canBeGrabbed: grabFlag description: aDescription [
        name := aName.
        location := aRoom.
        isAlive := true.
        canBeGrabbed := grabFlag.
        isGrabbed := false.
        investigated := false.
        investigationLine := 'They have nothing to say.'.
        description := aDescription.
        ^ self
    ]

    getName [
        ^ name
    ]

    describe [
        ^ description.
    ]

    location [ ^ location ]

    moveTo: aLocation [
        location := aLocation.
    ]

    moveTo: aRoom [
        Transcript show: 'You enter ', aRoom getName, '.'; cr.
        location := aRoom.
    ]

    investigate: player [
        | playerLocation|
        playerLocation := player location.

        (location = playerLocation) ifFalse: [
            Transcript show: 'You cant investigate, because ', name, ' is not here'; cr.
            ^ self.
        ].

        (isAlive) ifFalse: [
            Transcript show: 'There''s no point investigating the dead.'; cr.
            ^ self.
        ].

        investigated ifTrue: [
            Transcript show: '> ', name, ': I have nothing more to say.'; cr.
        ] ifFalse: [
            investigationLine do: [:line | Transcript show: line; cr].
            investigated := true.
        ].
    ]


    grab: player [
        (player location = location)
            ifFalse: [
                Transcript show: 'You are not in the same room as ', name, '.'; cr.
                ^ self
            ].
        isGrabbed
            ifTrue: [
                Transcript show: name, ' was already grabbed.'; cr.
                ^ self
            ].
        canBeGrabbed
            ifTrue: [
                isGrabbed := true.
                Transcript show: name, ' will go with you.'; cr.
            ]
            ifFalse: [
                Transcript show: name, ': What are you doing? Get your hands off me!'; cr.
            ].
        ^ self
    ]
]


Object subclass: Item [
    | name location isTaken |

    Item class >> name: aName location: aRoom [
        ^super new initName: aName location: aRoom
    ]

    initName: aName location: aRoom [
        name := aName.
        location := aRoom.
        isTaken := false.
        ^ self
    ]

    getName [
        ^ name
    ]

    takeBy: player [
        isTaken
            ifTrue: [
                Transcript show: name , ' is already taken.'; cr.
            ]
            ifFalse: [
                (player location = location)
                    ifTrue: [
                        isTaken := true.
                        Transcript show: 'You take ' , name; cr.
                    ]
                    ifFalse: [
                        Transcript show: name , ' is not here.'; cr.
                    ].
            ].
    ]
]


Object subclass: Game [
    | isGameOver rooms items crew player medbay powerRoom technicalRoom livingQuarters storageBay shuttle lightsOn forceInvestigation blockedInvestigation noisesHeard beckerChoice hintCounter countdown reed kendle walker douglas becker fluff gun multitool shuttleLocked |


    introductionText [
        ^ #(
            'You are the Diagnostics Officer aboard the spaceship Talume, on a mission to investigate a newly discovered life form. But something has gone horribly wrong - and the alien creature may not be the only danger lurking in the ship''s dark corridors...'
            'But before you continue your journey...'
            ''
        )
    ]

    instructionsText [
        ^ #(
            'Here is the list of available commands:'
            'move Room                  - to enter the Room.'
            'look                      - to look around the room.'
            'take Object               - to take an Object.'
            'investigate Person        - to investigate a crew member.'
            'crew                      - to see the list of crew members.'
            'rooms                     - to see the list of the rooms.'
            'exit                      - to exit the game.'
            'help                      - to see this message again.'
            ''
        )
    ]

    init [
        isGameOver := false.
        lightsOn := true.
        forceInvestigation := false.
        blockedInvestigation := false.
        noisesHeard := false.
        beckerChoice := nil.
        hintCounter := 0.
        countdown := nil.

        rooms := Dictionary new.
        medbay := Room
            name: 'Medbay'
            isOpen: true
            description: 'Beckers body is torn apart on the floor, blood pooling in dark, jagged patterns. The black substance spreads across the tiles, darker and thicker than in Douglas murder. A faint meowing echoes from a cupboard.'
            helpDescription: 'medical bay with an internal isolation space.'.
        powerRoom := Room
            name: 'PowerRoom'
            isOpen: true
            description: 'The power room hums with machinery. Flickering panels cast shifting shadows, and the air smells faintly of burnt metal.'
            helpDescription: 'controls the ship''s entire power system.'.
        technicalRoom := Room
            name: 'TechnicalRoom'
            isOpen: true
            description: 'The servers hum steadily. NAVCOREs screen glows softly, waiting silently for your next command'
            helpDescription: 'houses the main computer, NAVCORE.'.

        livingQuarters := Room
            name: 'LivingQuarters'
            isOpen: true
            description: 'The beds are neatly made, the desks empty, and everything seems in order - except for the body lying in the middle of the room.'
            helpDescription: 'where the crew sleeps and eats.'.
        storageBay := Room
            name: 'StorageBay'
            isOpen: true
            description: 'Rows of shelves line the room, scattered with guns catching the dim light, silent and waiting for you to grab one.'
            helpDescription: 'storage for weapons and canned supplies.'.
        shuttle := Room
            name: 'Shuttle'
            isOpen: false
            description: 'A shuttle for code-red escapes.'
            helpDescription: 'escape vessel for emergency departure.'.

        rooms at: 'Medbay' put: medbay; at: 'PowerRoom' put: powerRoom; at: 'TechnicalRoom' put: technicalRoom;
              at: 'LivingQuarters' put: livingQuarters; at: 'StorageBay' put: storageBay; at: 'Shuttle' put: shuttle.


        crew := Dictionary new.
        reed := CrewMember
                name: 'Reed'
                location: livingQuarters
                canBeGrabbed: true
                description: 'science officer.'.
        kendle := CrewMember
                name: 'Kendle'
                location: livingQuarters
                canBeGrabbed: false
                description: 'navigator.'.
        walker := CrewMember
                name: 'Walker'
                location: powerRoom
                canBeGrabbed: false
                description: 'chief engineer.'.
        douglas := CrewMember
                name: 'Douglas'
                location: livingQuarters
                canBeGrabbed: false
                description: 'captain of the Talume spaceship.'.
        becker := CrewMember
                name: 'Becker'
                location: medbay
                canBeGrabbed: false
                description: 'executive officer.'.
        crew at: 'Reed' put: reed; at: 'Kendle' put: kendle; at: 'Walker' put: walker; at: 'Douglas' put: douglas; at: 'Becker' put: becker.

        reed investigationLine: #(
            'What were you doing just now, Reed?'
            'I was running an analysis of Becker''s blood, he says. Trying to figure out how to help him... his condition isn''t improving.'
            'Did you find anything?'
            'Not exactly, Reed admits. But something''s wrong. His blood is more acidic than normal, and it''s not clotting at all.'
        ).

        kendle investigationLine: #(
            'You were the first here, right, Kendle? Did you notice anything?'
            'Not much, she says, voice trembling. The cat ran between my legs, yowling just before the scream.'
            'I don’t know who or what could have done this.'
        ).

        items := Dictionary new.
        fluff := Item name: 'Fluff' location: medbay.
        gun := Item name: 'Gun' location: storageBay.
        multitool := Item name: 'Multitool' location: medbay.
        items at: 'Fluff' put: fluff; at: 'Gun' put: gun; at: 'Multitool' put: multitool.

        player := CrewMember
                name: 'Player'
                location: technicalRoom
                canBeGrabbed: false
                description: 'you, the diagnostics officer.'.
    ]

    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
       self printLines: self introductionText.
    ]

    printInstructions [
        self printLines: self instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    beckerChoiceScene [
        self printLines: #(
            ''
            'NAVCORE, main spaceship''s computer hums softly when it prints the response on the screen.'
            'NAVCORE: Hello, Diagnostics Officer Pierce. Here is the report of Mission 067801'
            '"""'
            'Corporate command authorized the spaceship Talume to investigate a possible life form on planet 26-Draconis. Talume landed on the surface of 26-Draconis. Executive Officer Becker and Science Officer Reed left the ship to investigate. After 6 hours, Talume lost contact with Executive Officer Becker. Science Officer Reed reported unsuccessful search attempts.'
            '"""'
            'NAVCORE: Anything else I can do for you, Officer?'
            'Before you can respond, the main console clears. A new line appears.'
            'NAVCORE: Science Officer Reed has re-entered the Talume carrying the sick and unconcious Executive Officer Becker. His spacesuit is breached. Per quarantine law, the crew must be contained. Should I send the command to move him to the medbay for treatment, or to isolation to prevent potential contamination?'
            ''
            'Type either "medbay" or "isolation".'
            ''
        ).

        beckerChoice := self readCommand.
        [(beckerChoice = 'medbay') or: [beckerChoice = 'isolation']] whileFalse: [
            Transcript show: 'Type either isolation or medbay'; cr.
            beckerChoice := self readCommand.
        ].

        (beckerChoice = 'isolation')
            ifTrue: [hintCounter := hintCounter + 1].

        self printLines: {
            ''.
           'NAVCORE: Your choice has been recorded: ', beckerChoice, '. Command sent.'
        }.

        self powerOffScene.
    ]

    powerOffScene [
        self printLines: #(
           'You stretch and rise from the console, planning to look for the ship''s cat, Fluff.'
           'As you step into the corridor, the lights go out. The ship is plunged into darkness. The only sound is your own heartbeat, pounding in your ears. Your breath catches when you hear a scream - and stops entirely when it''s cut short.'
           'You remember the emergency procedure: in a total blackout, all crew members are to gather in the living quarters.'
       ).
       lightsOn := false.
   ]

   firstBodyScene [
        self printLines: {
            'You step into the living quarters. The lights flicker back on, blinding you for a moment.'.
            'A scream cuts through the silence - Kendle''s. As your eyes adjust, you see it: a body sprawled in the middle of the room, torn open, blood spreading across the floor like a shadow. It''s Douglas - your captain and friend.'.
            'Only Kendle and Reed are here. With the captain dead and Becker still unconscious, the only one unaccounted for is Walker, the chief engineer - he must''ve restored the power.'.
            'Reed turns to you, his voice tight. ''You''re the one in command now, Pierce. What do we do?'''.
            'Do you look around the room first, or investigate one of the crew members?'
        }.
        lightsOn := true.
        forceInvestigation := true.
        douglas isAlive: false.
    ]

    walkerJoinsScene [
        walker location: livingQuarters.
        self printLines: {
            'Before you can decide what to do next,  Walker - chief engineer - bursts in.'.
            '"I fixed the po-" he stops, startled by Douglas''s dead body. "What the hell happened here?"'.
            'The captain''s dead," Reed says. "Where have you been?"'.
            'When the lights went out, I went to the power room to restore them, Walker explains. I didn''t expect two people to die while I was gone.'.
            'Wait, did he just say two?'
        }.
        (beckerChoice = 'isolation')
            ifTrue: [walker investigationLine: #(
                '"I was fixing the power after it went out," he says. "Then I wanted to go straight to our quarters, but the medbay door was open and there was blood everywhere. So I went to check the isolation, and..."'
                '"Did Becker leave quarantine?"'
                '"Not exactly," Walker replies. "He''s still in isolation – but he''s dead, Pierce. Blood everywhere, his body torn apart. The strange thing is, no alarm went off, so it wasn''t a malfunction. Someone on the crew must have unlocked the door."'

            )]
            ifFalse: [
                walker investigationLine: #(
                '"What do you mean by two people, Walker?"'
                '"I was fixing the power after it went out", he says. "Then I wanted to go straight to our quarters, but the medbay door was open. There was blood everywhere - Becker''s blood, I think. I didn''t want to investigate without you."'
                )
            ].
    ]

    secondBodyScene [
        reed location: medbay.
        hintCounter := hintCounter + 1.
        becker isAlive: false.
        blockedInvestigation := true.
        noisesHeard := true.
        self printLines: {
            'Becker lies collapsed on the medbay floor - or rather, what''s left of him does. His body has been hollowed out completely, reduced to a deflated shell as if something had crawled inside him, worn him, and then peeled him off like clothing.The black substance from before slicks every surface, thicker now, spreading across the tiles like living oil.'.
            'A faint meow breaks the silence. Fluff peers out from a cupboard, fur bristling, eyes locked on the floor as if urging you to notice something. You follow his gaze and spot a discarded multitool beside the cupboard.'.
            'A sudden scream echoes from the power room, followed by a harsh mechanical noise. Your breath catches.'.
            'The door swings open. Reed steps inside, pale and grim.'.
            '"And then there were two," he whispers. "There''s one more body to find... and the killer."'.
            '"How do I know you''re not the killer?"'.
            '"You don''t," he admits. "But I can go with you to investigate - or you can go alone."'
        }.
        (hintCounter >= 2)
            ifTrue: [
                self printLines: {
                    'At this point, you''re certain someone on the crew is working with the alien. It could be Reed - but if it were, why hasn''t he killed you yet?'.
                    'Better not to split up when there might be another enemy aboard.'
                }
            ].
        Transcript show: 'If you want to take Reed with you, type ''grab Reed'' before going to the next room.'; cr.
    ]

    performMove: targetRoomName player: aPlayer [
        | targetRoom moveActionPerformed |

        targetRoom := rooms at: targetRoomName ifAbsent: [
            Transcript show: 'Room not found.'; cr. ^ self
        ].

        moveActionPerformed := false.

        (targetRoomName = 'Shuttle' and: [targetRoom canPlayerEnter not]) ifTrue: [
            countdown isNil ifTrue: [
                Transcript show: 'Cannot enter the shuttle. Only available in case of code red.'; cr.
            ] ifFalse: [
                Transcript show: 'The shuttle doors are closed. They shouldnt be - but the saboteur you failed to catch must have locked them. You remember the capitan also had the key. You run to headquarters and grab it. The whole ordeal takes over two minutes though...'; cr.
                countdown := countdown - 2.
                (countdown < 0)
                    ifTrue: [ Transcript show: 'TODO: autodestruction scene.'; cr. isGameOver := true. ]
                    ifFalse: [ Transcript show: 'TODO: Enter shuttle scene'; cr. player moveTo: targetRoom. ].
            ].
            moveActionPerformed := true.
        ].

        (moveActionPerformed not and: [beckerChoice isNil]) ifTrue: [
            Transcript show: 'NAVCORE is waiting for your decision. You can''t leave yet.'; cr.
            moveActionPerformed := true.
        ].


        (moveActionPerformed not and: [player location getName = targetRoomName]) ifTrue: [
            Transcript show: 'You are already here.'; cr.
            moveActionPerformed := true.
        ].

        (moveActionPerformed not and: [forceInvestigation]) ifTrue: [
            Transcript show: '"Where the hell are you going, Pierce?" - Reed snaps, grabbing your arm and pulling you back into the room. - "Youre not leaving until we figure out what happened here. Youre the warrant officer - you lead the investigation."'; cr.
            moveActionPerformed := true.
        ].

        (moveActionPerformed not and: [lightsOn not]) ifTrue: [
            moveActionPerformed := true.
            player moveTo: targetRoom.
            targetRoomName = 'LivingQuarters'
                ifTrue: [
                    self firstBodyScene.
                ]
                ifFalse: [ Transcript show: 'You almost trip over something you cant see. Everything is still pitch black.'; cr. ].
        ].

        (moveActionPerformed not and: [noisesHeard not]) ifTrue: [
            moveActionPerformed := true.
            player moveTo: targetRoom.
            targetRoomName = 'Medbay'
                ifTrue: [
                    self secondBodyScene.
                ].
            targetRoomName = 'PowerRoom'
                ifFalse: [
                    Transcript show: 'Scene: hearing noices'; cr.
                    noisesHeard := true.
                ].
        ].

        (moveActionPerformed not and: [countdown isNil]) ifTrue: [
            moveActionPerformed := true.
            player moveTo: targetRoom.
            targetRoomName = 'PowerRoom' ifTrue: [
                Transcript show: 'Scene: confrontationScene'; cr.
                countdown := 3.
                shuttleLocked := false.
            ] ifFalse: [
                Transcript show: 'Scene: ignoring noises'; cr.
                countdown := 3.
            ].
        ].

        (moveActionPerformed not) ifTrue: [
            player moveTo: targetRoom.
            countdown notNil ifTrue: [
                countdown := countdown - 1.
                targetRoomName = 'Shuttle' ifTrue: [
                    Transcript show: 'Scene: enter shuttle'; cr.
                ].
                countdown <= 0 ifTrue: [
                    Transcript show: 'Scene: autoDestructionScene'; cr.
                ].
                Transcript show: 'Player can go to max 3 rooms, with shuttle space being the last. Current counter: ', countdown printString, '.'; cr.
            ].
        ].
    ]

    performInvestigation: crewMemberName player: aPlayer [
        | crewMember |
        crewMember := crew at: crewMemberName ifAbsent: [
            Transcript show: 'Person not found. Use uppercase.'; cr. ^ self
        ].
        (blockedInvestigation)
            ifTrue: [
                Transcript show: 'There''s no time to waste on talking now.'; cr. ^ self
            ].
        crewMember investigate: aPlayer.
        (crewMemberName = 'Kendle') ifTrue: [
            forceInvestigation := false.
            self walkerJoinsScene.
        ].
    ]

    performTakingItem: itemName player: aPlayer [
        | item |
        item := items at: itemName ifAbsent: [
            Transcript show: 'Item not found. Use uppercase.'; cr. ^ self
        ].
        item takeBy: aPlayer.
    ]

    performGrab: crewMemberName player: aPlayer [
        | crewMember |
        crewMember := crew at: crewMemberName ifAbsent: [
            Transcript show: 'Person not found. Use uppercase.'; cr. ^ self
        ].
        crewMember grab: aPlayer.
    ]

    run [
        | cmd keyPressed |
        self init.
        self printIntroduction.
        self printInstructions.

        self beckerChoiceScene.

        [isGameOver] whileFalse: [
            | isUnknown cName iName |
            isUnknown := true.
            cmd := self readCommand.

            cmd = 'help' ifTrue: [
                self printInstructions.
                isUnknown := false
            ].

            cmd = 'rooms' ifTrue: [
                rooms keys do: [ :roomName |
                    Transcript show: roomName, ' - ', (rooms at: roomName) describe; cr
                ].
                isUnknown := false
            ].

            cmd = 'crew' ifTrue: [
                crew keys do: [ :crewName |
                    Transcript show: crewName, ' - ', (crew at: crewName) describe; cr
                ].
                isUnknown := false
            ].

            (cmd = 'look') ifTrue: [
                isUnknown := false.
                (lightsOn) ifFalse: [
                    Transcript show: 'You can''t see anything'; cr.
                ] ifTrue: [
                    player location look.
                ]
            ].

            ((cmd indexOfSubCollection: 'investigate' startingAt: 1) > 0) ifTrue: [
               cName := (cmd copyFrom: 13 to: cmd size).
               self performInvestigation: cName player: player.
               isUnknown := false
            ].

           ((cmd indexOfSubCollection: 'move' startingAt: 1) > 0) ifTrue: [
               | rName |
               rName := (cmd copyFrom: 6 to: cmd size).
               self performMove: rName player: player.
               isUnknown := false
           ].

           ((cmd indexOfSubCollection: 'take' startingAt: 1) > 0) ifTrue: [
               iName := (cmd copyFrom: 6 to: cmd size).
               self performTakingItem: iName player: player.
               isUnknown := false
           ].

           ((cmd indexOfSubCollection: 'grab' startingAt: 1) > 0) ifTrue: [
               cName := (cmd copyFrom: 6 to: cmd size).
               self performGrab: cName player: player.
               isUnknown := false
            ].

            cmd = 'exit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].

            isUnknown ifTrue: [
                self printLines: #('Unknown command.')
            ].
        ].
    ]
]


Game new run.
