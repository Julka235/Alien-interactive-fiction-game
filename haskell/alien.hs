import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)

-- rooms
data RoomType
    = PowerRoom
    | TechnicalRoom
    | LivingQuarters
    | Medbay
    | StorageBay
    | Shuttle
    deriving (Enum, Eq, Show)

-- things
data ThingType
    = Fluff
    | Gun
    | Multitool
    deriving (Enum, Eq)

-- characters
data CharacterType
    = Lambert
    | Dallas
    | Becker
    | Walker
    | Reed
    deriving (Enum, Eq)

-- valid choose
data ChooseType
    = MedBay
    | Isolation
    deriving (Enum, Eq)

-- parsing part
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
parseCharacter "Lambert"    = Just Lambert
parseCharacter "Dallas"     = Just Dallas
parseCharacter "Becker"     = Just Becker
parseCharacter "Walker"     = Just Walker
parseCharacter "Reed"       = Just Reed
parseCharacter _            = Nothing

parseChoose :: String -> Maybe ChooseType
parseChoose "MedBay"           = Just MedBay
parseChoose "Isolation"        = Just Isolation
parseChoose _                  = Nothing

-- describe rooms
describe :: RoomType -> String

describe Medbay = "Becker's body is torn apart on the floor, blood pooling in dark, jagged patterns. The black substance spreads across the tiles, darker and thicker than in Dallas's murder. A faint meowing echoes from a cupboard.\n"
describe LivingQuarters = "The beds are neatly made, the desks empty, and everything seems in order - except for the body lying in the middle of the room.\n"
describe PowerRoom = "The power room hums with machinery. Flickering panels cast shifting shadows, and the air smells faintly of burnt metal.\n"
describe TechnicalRoom = "The servers hum steadily. MU/TH/ER's screen glows softly, waiting silently for your next command.\n"
describe StorageBay = "Rows of shelves line the room, scattered with guns catching the dim light, silent and waiting for you to grab one.\n"
describe _ = "There is no such room.\n"

-- investigation responses
investigationResponse :: CharacterType -> ChooseType -> String

investigationResponse Lambert _ =
    "'You were the first here, right, Lambert?'\n"
    ++ "'Yeah,' she says, voice trembling. 'I was walking down the corridor when the power went out. Then I heard the scream and...' She glances tearfully at Dallas’ body. 'I don’t know who or what could have done this.'\n"
    ++ "'Did you notice anything else?'\n"
    ++ "'Not much,' she says, shivering. 'Except for Fluff. The cat ran between my legs just before the scream. He was clearly rattled, yowling and howling.'\n"
    ++ "'Fluff’s instincts were always sharp,' you think to yourself. 'If he sensed danger before anyone else... maybe he’s seen what we haven’t.'\n"

investigationResponse Reed _ =
    "'What were you doing just now, Reed?'\n"
    ++ "'I was running an analysis of Becker's blood,' he says. 'Trying to figure out how to help him... his condition isn't improving.'\n"
    ++ "'Did you find anything?'\n"
    ++ "'Not exactly,' Reed admits. 'But something's wrong. His blood is more acidic than normal, and it's not clotting at all.'\n"

investigationResponse Walker MedBay =
    "'What do you mean by two people, Walker?'\n"
    ++ "'I was fixing the power after it went out,' he says. 'Then I wanted to go straight to our quarters, but the medbay door was open. There was blood everywhere – Becker's blood, I think. I didn't want to investigate without you.'\n"

investigationResponse Walker Isolation =
    "'I was fixing the power after it went out,' he says. 'Then I wanted to go straight to our quarters, but the medbay door was open and there was blood everywhere. So I went to check the isolation, and...'\n"
    ++ "'Did Becker leave quarantine?'\n"
    ++ "'Not exactly,' Walker replies. 'He's still in isolation – but he's dead, Ripley. Blood everywhere, his body torn apart. The strange thing is, no alarm went off, so it wasn't a malfunction. Someone on the crew must have unlocked the door.'\n"

-- current world state
data WorldState = WorldState
  {
    inventory          :: [ThingType]
  , currentRoom        :: RoomType
  , roomThings         :: [(ThingType, RoomType)]
  , roomCharacters     :: [(CharacterType, RoomType)]
  , deadCharacters     :: [CharacterType]
  , countdown          :: Int
  , hintCounter        :: Int
  , beckerChoice       :: Maybe ChooseType
  , quartersInvestigated :: Bool
  , investigated       :: [CharacterType]
  , gameOver           :: Bool
  }

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
      , (Lambert, LivingQuarters)
      , (Walker, PowerRoom)
      , (Dallas, LivingQuarters)
      , (Becker, Medbay)
      ]
  , deadCharacters =
      [ Dallas
      , Becker
      ]
  , countdown = 3
  , hintCounter = 0
  , beckerChoice = Nothing
  , quartersInvestigated = False
  , investigated = []
  , gameOver = False
  }

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
    return ()

-- crew
printCrew :: IO ()
printCrew = do
    putStrLn "Here is the list of the members of your crew:"
    putStrLn "Fluff      - spaceship's cat."
    putStrLn "Dallas     - captain of the Nostromo spaceship."
    putStrLn "Lambert    - navigator."
    putStrLn "Walker     - chief engineer."
    putStrLn "Becker     - executive officer."
    putStrLn "Reed       - science officer."
    return ()

-- rooms
printRooms :: IO ()
printRooms = do
    putStrLn "Here is the list of rooms aboard the Nostromo spaceship."
    putStrLn "LivingQuarters    - where the crew sleeps and eats."
    putStrLn "Medbay            - medical bay with an internal isolation space."
    putStrLn "StorageBay        - storage for weapons and canned supplies."
    putStrLn "TechnicalRoom     - houses the main computer, MU/TH/ER."
    putStrLn "PowerRoom         - controls the ship's entire power system."
    putStrLn "Shuttle           - escape vessel for emergency departure."
    return ()

start :: IO ()
start = do
    putStrLn "Do you want to play a game?'), nl,"
    putStrLn "You are the Warrant Officer aboard the spaceship Nostromo, on a mission to investigate a newly discovered life form. But something  has gone horribly wrong - and the alien creature may not be the only danger lurking in the ship's dark corridors..."
    putStrLn "But before you continue your journey:"
    printHelp
    putStrLn ""
    putStrLn "MU/TH/ER, main spaceship's computer hums softly when it prints the response on the screen."
    putStrLn "MU/TH/ER: Hello, Warrant Officer Ripley. Here is the report you requested."
    putStrLn "Report of Mission 067801"
    putStrLn "Time 9036727h: Corporate command authorizes the spaceship Nostromo to investigate a possible life form on planet 26-Draconis."
    putStrLn "Time 9036911h: Nostromo lands on the surface of 26-Draconis. Executive Officer Becker and Science Officer Reed leave the ship to investigate."
    putStrLn "Time 9036916h: Nostromo loses contact with Executive Officer Becker. Science Officer Reed reports unsuccessful search attempts."
    putStrLn "MU/TH/ER: Anything else I can do for you, Officer?"
    putStrLn "Before you can respond, the main console clears. A new line appears."
    putStrLn "MU/TH/ER: Science Officer Reed has re-entered the Nostromo carrying the sick and unconcious Executive Officer Becker. His spacesuit is breached. Per quarantine law, the crew must be contained. Should I send the command to move him to the medbay for treatment, or to isolation to prevent potential contamination?"
    putStrLn "Type either 'Choose MedBay' or 'Choose Isolation'."