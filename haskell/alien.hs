import Data.Maybe (fromMaybe)

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

-- valid put
data PutType
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

parsePut :: String -> Maybe PutType
parsePut "MedBay"           = Just MedBay
parsePut "Isolation"        = Just Isolation
parsePut _                  = Nothing

-- instructions

-- describe rooms
describe :: RoomType -> String

describe Medbay = "Becker's body is torn apart on the floor, blood pooling in dark, jagged patterns. The black substance spreads across the tiles, darker and thicker than in Dallas's murder. A faint meowing echoes from a cupboard.\n"
describe LivingQuarters = "The beds are neatly made, the desks empty, and everything seems in order - except for the body lying in the middle of the room.\n"
describe PowerRoom = "The power room hums with machinery. Flickering panels cast shifting shadows, and the air smells faintly of burnt metal.\n"
describe TechnicalRoom = "The servers hum steadily. MU/TH/ER's screen glows softly, waiting silently for your next command.\n"
describe StorageBay = "Rows of shelves line the room, scattered with guns catching the dim light, silent and waiting for you to grab one.\n"

-- investigation responses
investigationResponse :: CharacterType -> PutType -> String

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
