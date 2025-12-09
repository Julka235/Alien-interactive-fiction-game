import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)

-- | rooms
data RoomType
    = PowerRoom
    | TechnicalRoom
    | LivingQuarters
    | Medbay
    | StorageBay
    | Shuttle
    deriving (Enum, Eq, Show)

-- | things
data ThingType
    = Fluff
    | Gun
    | Multitool
    deriving (Enum, Eq, Show)

-- | characters
data CharacterType
    = Kendle
    | Douglas
    | Becker
    | Walker
    | Reed
    deriving (Enum, Eq, Show)

-- | valid choose
data ChooseType
    = MedBay
    | Isolation
    deriving (Enum, Eq, Show)

-- | parsing part
parseRoom :: String -> Maybe RoomType
parseRoom "PowerRoom"       = Just PowerRoom
parseRoom "TechnicalRoom"   = Just TechnicalRoom
parseRoom "LivingQuarters"  = Just LivingQuarters
parseRoom "Medbay"          = Just Medbay
parseRoom "StorageBay"      = Just StorageBay
parseRoom "Shuttle"         = Just Shuttle
parseRoom _                 = Nothing

parseThing :: String -> Maybe ThingType
parseThing "Fluff"          = Just Fluff
parseThing "Gun"            = Just Gun
parseThing "Multitool"      = Just Multitool
parseThing _                = Nothing

parseCharacter :: String -> Maybe CharacterType
parseCharacter "Kendle"    = Just Kendle
parseCharacter "Douglas"     = Just Douglas
parseCharacter "Becker"     = Just Becker
parseCharacter "Walker"     = Just Walker
parseCharacter "Reed"       = Just Reed
parseCharacter _            = Nothing

parseChoose :: String -> Maybe ChooseType
parseChoose "MedBay"           = Just MedBay
parseChoose "Isolation"        = Just Isolation
parseChoose _                  = Nothing

-- | describe rooms
describe :: RoomType -> String

describe Medbay = "The beds are neatly made, the desks empty, and everything seems in order - except for the body lying in the middle of the room."
describe LivingQuarters = "The beds are neatly made, the desks empty, and everything seems in order - except for the body lying in the middle of the room."
describe PowerRoom = "The power room hums with machinery. Flickering panels cast shifting shadows, and the air smells faintly of burnt metal."
describe TechnicalRoom = "The servers hum steadily. NAVCORE's screen glows softly, waiting silently for your next command."
describe StorageBay = "Rows of shelves line the room, scattered with guns catching the dim light, silent and waiting for you to grab one."
describe _ = "There is no such room."

-- | investigation responses
investigationResponse :: CharacterType -> ChooseType -> String

investigationResponse Kendle _ =
    "'You were the first here, right, Kendle? Did you notice anything?'\n"
    ++ "'Not much,' she says, voice trembling. 'I was walking down the corridor when the power went out. Then the cat ran between my legs, yowling and howling, just before the scream. And then...' She glances tearfully at Douglas’ body and shivers. 'I don’t know who or what could have done this.'\n"
    ++ "'Fluff’s instincts were always sharp,' you think to yourself. 'If he sensed danger before anyone else... maybe he’s seen what we haven’t.'"

investigationResponse Reed _ =
    "'What were you doing just now, Reed?'\n"
    ++ "'I was running an analysis of Becker's blood,' he says. 'Trying to figure out how to help him... his condition isn't improving.'\n"
    ++ "'Did you find anything?'\n"
    ++ "'Not exactly,' Reed admits. 'But something's wrong. His blood is more acidic than normal, and it's not clotting at all.'"

investigationResponse Walker MedBay =
    "'What do you mean by two people, Walker?'\n"
    ++ "'I was fixing the power after it went out,' he says. 'Then I wanted to go straight to our quarters, but the medbay door was open. There was blood everywhere – Becker's blood, I think. I didn't want to investigate without you.'"

investigationResponse Walker Isolation =
    "'I was fixing the power after it went out,' he says. 'Then I wanted to go straight to our quarters, but the medbay door was open and there was blood everywhere. So I went to check the isolation, and...'\n"
    ++ "'Did Becker leave quarantine?'\n"
    ++ "'Not exactly,' Walker replies. 'He's still in isolation – but he's dead, Pierce. Blood everywhere, his body torn apart. The strange thing is, no alarm went off, so it wasn't a malfunction. Someone on the crew must have unlocked the door.'"


-- | current world state
data WorldState = WorldState
  {
    inventory          :: [ThingType]
  , currentRoom        :: RoomType
  , roomThings         :: [(ThingType, RoomType)]
  , roomCharacters     :: [(CharacterType, RoomType)]
  , deadCharacters     :: [CharacterType]
  , lights             :: Bool
  , countdown          :: Int
  , hintCounter        :: Int
  , beckerChoice       :: Maybe ChooseType
  , forceInvestigation :: Bool
  , quartersInvestigated :: Bool
  , investigated       :: [CharacterType]
  , shuttleClosed      :: Bool
  , gameOver           :: Bool
  }

-- | starting world state
initialWorldState :: WorldState
initialWorldState = WorldState
  { inventory = []
  , currentRoom = TechnicalRoom
  , roomThings =
      [ (Fluff, Medbay)
      , (Multitool, Medbay)
      , (Gun, StorageBay)
      ]
  , roomCharacters =
      [ (Reed, LivingQuarters)
      , (Kendle, LivingQuarters)
      , (Walker, PowerRoom)
      , (Douglas, LivingQuarters)
      , (Becker, Medbay)
      ]
  , deadCharacters =
      [ Douglas
      , Becker
      ]
  , lights = True
  , countdown = 3
  , hintCounter = 0
  , beckerChoice = Nothing
  , forceInvestigation = False
  , quartersInvestigated = False
  , investigated = []
  , shuttleClosed = True
  , gameOver = False
  }

-- | game loop logic
gameLoop :: WorldState -> IO ()
gameLoop ws
    | gameOver ws = putStrLn "GAME OVER. Hope you join the Talume again soon."
    | otherwise = do
        putStr "> "
        hFlush stdout
        input <- getLine
        ws' <- handleCommand input ws
        gameLoop ws'

-- | handling command line commands
handleCommand :: String -> WorldState -> IO WorldState
handleCommand input ws =
    case words input of
        ["Exit"] ->
            return ws { gameOver = True }

        ["Help"] -> do
            printHelp
            return ws

        ["Rooms"] -> do
            printRooms
            return ws

        ["Crew"] -> do
            printCrew
            return ws

        ["Look"] -> do
            if not (lights ws) then do
                putStrLn "You can't see anything. It's completely dark."
                return ws

            else if not (quartersInvestigated ws) && currentRoom ws == LivingQuarters then do
                putStrLn "The blood is still spreading across the floor. A strange black substance mixes with it in the wounds. They don’t look human-made — jagged and mangled, as if something with a jaw full of sharp teeth tore them."
                return ws { quartersInvestigated = True }

            else do
                putStrLn (describe (currentRoom ws))
                return ws

        ["Go", roomStr] ->
            handleGo roomStr ws

        ["Take", thingStr] ->
            handleTake thingStr ws

        ["Investigate", charStr] ->
            handleInvestigate charStr ws

        ["Choose", choiceStr] ->
            handleChooseCommand choiceStr ws

        _ -> do
            putStrLn "Unknown command. Type Help."
            return ws

-- | display message for choice what to do with Becker 
applyChoice :: ChooseType -> WorldState -> (String, WorldState)
applyChoice choice ws
    | beckerChoice ws /= Nothing =
        ("You already made your choice. It is too late to change it now.\n", ws)
    | otherwise =
        let wsChosen = ws { beckerChoice = Just choice }
            wsHandled = handleChoiceEffects choice wsChosen
            (powerMsg, wsFinal) = powerOffScene wsHandled
            msg = "NAVCORE: Your choice has been recorded: " ++ show choice ++ ". Command sent.\n" ++ powerMsg
        in (msg, wsFinal)

handleChoiceEffects :: ChooseType -> WorldState -> WorldState
handleChoiceEffects Isolation ws =
    ws { hintCounter = hintCounter ws + 1 }
handleChoiceEffects MedBay ws =
    ws

-- | logic for choice with Becker
handleChooseCommand :: String -> WorldState -> IO WorldState
handleChooseCommand arg ws =
    case parseChoose arg of
        Nothing -> do
            putStrLn "You must choose MedBay or Isolation."
            putStrLn "Try again."
            return ws
        Just choice -> do
            let (msg, ws') = applyChoice choice ws
            putStrLn msg
            return ws'

-- | lights go out scene
powerOffScene :: WorldState -> (String, WorldState)
powerOffScene ws =
    let msg = "You stretch and rise from the console, planning to look for the ship's cat, Fluff.\nAs you step into the corridor, the lights go out. The ship is plunged into darkness. The only sound is your own heartbeat, pounding in your ears. Your breath catches when you hear a scream - and stops entirely when it’s cut short.\nYou remember the emergency procedure: in a total blackout, all crew members are to gather in the living quarters."
        ws' = ws { lights = False }
    in (msg, ws')

-- | first body in living quarters
firstBodyScene :: WorldState -> IO WorldState
firstBodyScene ws = do
    putStrLn "You step into the living quarters. The lights flicker back on, blinding you for a moment."
    putStrLn "A scream cuts through the silence - Kendle's. As your eyes adjust, you see it: a body sprawled in the middle of the room, torn open, blood spreading across the floor like a shadow. It's Douglas - your captain and friend."
    putStrLn "Only Kendle and Reed are here. With the captain dead and Becker still unconscious, the only one unaccounted for is Walker, the chief engineer - he must've restored the power."
    putStrLn "Reed turns to you, his voice tight. 'You're the one in command now, Pierce. What do we do?'"
    putStrLn "Do you look around the room first, or investigate one of the crew members?"
    let ws' = ws
            { lights = True
            , forceInvestigation = True
            , deadCharacters = Douglas : deadCharacters ws
            }
    return ws'

-- | shutle locked 
shuttleLocked :: WorldState -> Bool
shuttleLocked ws = shuttleClosed ws

-- | are lights on
lightsOn :: WorldState -> Bool
lightsOn ws = lights ws

-- was Becker decision made
waitingForChoice :: WorldState -> Bool
waitingForChoice ws = beckerChoice ws == Nothing

-- | is player in the room
alreadyInRoom :: RoomType -> WorldState -> Bool
alreadyInRoom r ws = currentRoom ws == r

-- | are forcing player to investigate
mustInvestigate :: WorldState -> Bool
mustInvestigate ws = forceInvestigation ws

-- | main Go logic
handleGo :: String -> WorldState -> IO WorldState
handleGo roomStr ws =
    case parseRoom roomStr of
        Nothing -> do
            putStrLn "No such room."
            return ws
        Just r -> do
            if r == Shuttle && shuttleLocked ws then do
                putStrLn "Cannot enter the shuttle. Only available in case of code red."
                return ws
            else if waitingForChoice ws then do
                putStrLn "NAVCORE is waiting for your decision. You can't leave yet."
                return ws
            else if alreadyInRoom r ws then do
                putStrLn "You are already here."
                return ws
            else if mustInvestigate ws then do
                putStrLn "'Where the hell are you going, Pierce?' Reed snaps, grabbing your arm. \
                         \ 'You're not leaving until we figure out what happened here. \
                         \ You're the warrant officer - you lead the investigation.'"
                putStrLn ""
                return ws
            else do
                case r of
                    Medbay   -> putStrLn "You enter the Medbay, noticing the isolation space."
                    Shuttle  -> return ()  -- no message
                    _        -> putStrLn ("You enter the " ++ show r ++ ".")

                -- handle lights off scenario
                ws' <- if not (lightsOn ws) then
                           case r of
                               LivingQuarters -> do
                                   wsAfterScene <- firstBodyScene ws
                                   return (wsAfterScene { currentRoom = r })
                               _ -> do
                                   putStrLn "You almost trip over something you can’t see. Everything is still pitch black."
                                   return (ws { currentRoom = r })
                       else return (ws { currentRoom = r })

                return ws'

-- | take logic
handleTake :: String -> WorldState -> IO WorldState
handleTake thingStr ws = 
    case parseThing thingStr of
        Nothing -> do
            putStrLn "There is no such thing to take."
            return ws
        Just c ->
            let inRoom = case lookup c (roomThings ws) of
                            Just room -> room == currentRoom ws
                            Nothing -> False
                already = c `elem` inventory ws
            in if not inRoom then do
                    putStrLn "It is not here."
                    return ws
                else if already then do
                    putStrLn "You\'re already holding it!"
                    return ws
                else do
                    let ws' = ws { inventory = c : inventory ws}
                    putStrLn "Taken."
                    return ws'

-- | investigation logic
handleInvestigate :: String -> WorldState -> IO WorldState
handleInvestigate charStr ws =
    case parseCharacter charStr of
        Nothing -> do
            putStrLn $ "There is no such character as " ++ charStr ++ "."
            putStrLn "To see characters type 'Crew'."
            return ws

        Just c ->
            let inRoom = case lookup c (roomCharacters ws) of
                            Just room -> room == currentRoom ws
                            Nothing -> False
                alive = not (c `elem` deadCharacters ws)
                already = c `elem` investigated ws
            in if not inRoom then do
                   putStrLn "You can't investigate them - they're not in the room with you."
                   return ws
               else if not alive then do
                   putStrLn "There's no point investigating the dead."
                   return ws
               else if already then do
                   putStrLn "'I have nothing more to say.' is all they say."
                   return ws
               else do
                   let ws' = ws { investigated = c : investigated ws
                                , forceInvestigation = if c == Kendle then False else forceInvestigation ws
                                }
                   case c of
                       Kendle -> putStrLn (investigationResponse Kendle (fromMaybe MedBay (beckerChoice ws')))
                       Reed    -> putStrLn (investigationResponse Reed (fromMaybe MedBay (beckerChoice ws')))
                       Walker  -> putStrLn (investigationResponse Walker (fromMaybe MedBay (beckerChoice ws')))
                       _       -> putStrLn $ "You look at " ++ show c ++ ", but there is nothing special to note."

                   return ws'

-- help
printHelp :: IO ()
printHelp = do
    putStrLn "Here is the list of available commands:"
    putStrLn "Go Room               - to enter the Room."
    putStrLn "Look                  - to look around the room."
    putStrLn "Take Object           - to take an Object."
    putStrLn "Investigate Person    - to investigate a crew member."
    putStrLn "Crew                  - to see the list of crew members."
    putStrLn "Rooms                 - to see the list of the rooms."
    putStrLn "Exit                  - to exit the game."
    putStrLn "Help                  - to see this message again."
    hFlush stdout
    return ()

-- crew
printCrew :: IO ()
printCrew = do
    putStrLn "Here is the list of the members of your crew:"
    putStrLn "Fluff      - spaceship's cat."
    putStrLn "Douglas    - captain of the Talume spaceship."
    putStrLn "Kendle     - navigator."
    putStrLn "Walker     - chief engineer."
    putStrLn "Becker     - executive officer."
    putStrLn "Reed       - science officer."
    hFlush stdout
    return ()

-- rooms
printRooms :: IO ()
printRooms = do
    putStrLn "Here is the list of rooms aboard the Talume spaceship."
    putStrLn "LivingQuarters    - where the crew sleeps and eats."
    putStrLn "Medbay            - medical bay with an internal isolation space."
    putStrLn "StorageBay        - storage for weapons and canned supplies."
    putStrLn "TechnicalRoom     - houses the main computer, NAVCORE."
    putStrLn "PowerRoom         - controls the ship's entire power system."
    putStrLn "Shuttle           - escape vessel for emergency departure."
    hFlush stdout
    return ()

-- | Main or start logic
main :: IO ()
main = do
    putStrLn "Do you want to play a game?"
    _ <- getLine
    putStrLn "You are the Diagnostics Officer aboard the spaceship Talume, on a mission to investigate a newly discovered life form. But something  has gone horribly wrong - and the alien creature may not be the only danger lurking in the ship's dark corridors..."
    putStrLn "But before you continue your journey:"
    printHelp
    putStrLn ""
    putStrLn "NAVCORE, main spaceship's computer hums softly when it prints the response on the screen."
    putStrLn "NAVCORE: Hello, Diagnostics Officer Pierce. Here is the report of Mission 067801"
    putStrLn "Corporate command authorized the spaceship Talume to investigate a possible life form on planet 26-Draconis. Talume landed on the surface of 26-Draconis. Executive Officer Becker and Science Officer Reed left the ship to investigate. After 6 hours, Talume lost contact with Executive Officer Becker. Science Officer Reed reported unsuccessful search attempts."
    putStrLn "NAVCORE: Anything else I can do for you, Officer?"
    putStrLn "Before you can respond, the main console clears. A new line appears."
    putStrLn "NAVCORE: Science Officer Reed has re-entered the Talume carrying the sick and unconcious Executive Officer Becker. His spacesuit is breached. Per quarantine law, the crew must be contained. Should I send the command to move him to the medbay for treatment, or to isolation to prevent potential contamination?"
    putStrLn ""
    putStrLn "Type either 'Choose MedBay' or 'Choose Isolation'."
    hFlush stdout
    gameLoop initialWorldState