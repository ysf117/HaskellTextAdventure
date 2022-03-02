--Importing modules used in game 
import qualified Data.Map as Map
import Data.Maybe (fromJust)


-- Data type for RoomId
type RoomId = Int

-- Declaration for predefined actions
data PredefinedAction = 
  Look | 
  Take | 
  PickUp | 
  Attack | 
  Block |
  Parry |
  Open  |
  UseKey  |
  Search  |
  North | 
  South | 
  East | 
  West 


--Declaration for actions in game
data Action = PredefAction PredefinedAction | StringAction String

--Declaration for results in game
data Result = 
  PlayerDeath String | 
  GameWon String |
  SomethingHappened String |
  Move RoomId String |
  NothingHappened |
  IncorrectCombo

--Declaration for Items and player
data Item = Item String
data Player = Player { inventory :: [Item] }

--Type constructor for interacting with world objects
type Interaction = Action -> Result
data Object = Object { objectInteraction :: Interaction }

--Type constructor for Objects and targetted actions
type ObjectName = String
type TargettedAction = (ObjectName, Action)

-- Declaration for Rooms
data Room = 
  Room {
    text :: String,
    objects :: Map.Map String Object
  }


--Type constructor for GameMap

type GameMap = Map.Map RoomId Room

-- Command line parser

getPlayerAction :: IO TargettedAction
getPlayerAction = do
  text <- getLine
  let tokens = words text

  let action = 
                case tokens of
                  ("pick up":target:_) -> Just (target, PredefAction PickUp)
                  ("look":target:_) -> Just (target, PredefAction Look)
                  ("take":target:_) -> Just (target, PredefAction Take)
                  ("attack":target:_) -> Just (target, PredefAction Attack)
                  ("block":target:_) -> Just (target, PredefAction Block)
                  ("parry":target:_) -> Just (target, PredefAction Parry)
                  ("open":target:_) -> Just (target, PredefAction Open)
                  ("search":target:_) -> Just (target, PredefAction Search)
                  ("usekey":target:_) -> Just (target, PredefAction UseKey)
                  ("north":target:_) -> Just (target, PredefAction North)
                  ("south":target:_) -> Just (target, PredefAction South)
                  ("east":target:_) -> Just (target, PredefAction East)
                  ("west":target:_) -> Just (target, PredefAction West)
                  (act:target:_) -> Just (target, StringAction act)
                  ("look":[]) -> Just ("", PredefAction Look)
                  ("pick up":[]) -> Just ("", PredefAction PickUp)
                  ("take":[]) -> Just ("", PredefAction Take)
                  ("attack":[]) -> Just ("", PredefAction Attack)
                  ("block":[]) -> Just ("", PredefAction Block)
                  ("parry":[]) -> Just ("", PredefAction Parry)
                  ("usekey":[]) -> Just ("", PredefAction UseKey)
                  ("north":[]) -> Just ("", PredefAction North)
                  ("south":[]) -> Just ("", PredefAction South)
                  ("east":[]) -> Just ("", PredefAction East)
                  ("west":[]) -> Just ("", PredefAction West)
                  _ -> Nothing


                  -- Case guard for actions if word isnt a token.
  case action of
    Nothing -> do
      putStrLn "Not a valid action"
      getPlayerAction
    Just act -> return act

-- Function for evaluating the different types of actions.
evalAction :: Room -> IO (Maybe Result)
evalAction room = do
  (object, action) <- getPlayerAction

  let maybeObject = Map.lookup object $ objects room
  return $ (\object -> objectInteraction object $ action) <$> maybeObject

-- Function for results when interacting in rooms 
game :: GameMap -> RoomId -> IO ()
game gameMap roomId = do
  print . text . fromJust . Map.lookup roomId $ gameMap

  doAction
  where
    doAction = do
      maybeResult <- evalAction . fromJust . (Map.lookup) roomId $ gameMap
-- Case gaurds for in game results
      case maybeResult of 
        Nothing -> do
          putStrLn "No such thing in here!"
          doAction
        Just (PlayerDeath reason) -> do
          putStrLn $ "You died!" ++ reason
          return ()
        Just (GameWon reason) -> do
          putStrLn $ "Winner Winner, Chicken Dinner!" ++ reason
          return ()
        Just (SomethingHappened something) -> do
          putStrLn something
          doAction
        Just NothingHappened -> do
          putStrLn "Literally nothing happened"
          doAction
        Just IncorrectCombo -> do
          putStrLn "You ignite the corresponding torches, however they mysteriously turn off again"
          doAction
        Just (Move newRoomId something) -> do
          putStrLn something
          game gameMap newRoomId

-- Function for mapping out the game rooms, allocating each room to a number
gameMap :: GameMap
gameMap = Map.fromList $ [
    (0, room0),
    (1, room1),
    (2, room2),
    (3, room1a),
    (4, room3),
    (5, room3a),
    (6, room3b),
    (7, room1b),
    (8, room4),
    (9, room4a),
    (10, room4b),
    (11, room4c),
    (12, room4d),
    (13, room1c)
  ]
-- To start the game 
play = game gameMap 0

-- shortcut for testing levels
-- test = game gameMap 

-- Creating the Rooms
-- Structure: Initial string for the description of the room
--            Mapping all the objects in the room
--            Mapping different types of actions to objects in the room: 'object (actionType predefinedaction) mayberesult string'
--            Same structure for most of the rooms

-- Room 0 = Initial Dungeon Entrance
room0 = 
  Room 
    "As you walk down the narrow passage bearing a torch you enter a cryptic room, as you take a step inside you trigger a trap button on the ground causing the only exit to close."
    (Map.fromList $ [
      ("", Object room),
      ("door", Object door),
      ("soldier", Object soldier),
      ("sword", Object sword),
      ("shield", Object shield),
      ("torches", Object torches),
      ("barrel", Object barrel)
    ])
  where
    room (PredefAction Look) = SomethingHappened "A corpse of a soldier lies on the north wall below an unlit torch, his sword and shield lay next him. There is another door to the east of the room below a glowing red orb with two unlit torches beside it. There is a barrel on the south wall below an unlit torch and the closed exit on the west."
    room _ = NothingHappened

    door (PredefAction Look) = SomethingHappened "A solid concrete door, with a glowing red orb above."
    door (PredefAction Open) = SomethingHappened "You try to open the door but it won't budge."
    door _ = NothingHappened

    soldier (PredefAction Look) = SomethingHappened "A corpse of soldier with a crest of an ancient mercenary band, theres a pouch in its hands and a sword and shield lay next to the body"
    soldier (PredefAction Search) = SomethingHappened "You search the soldiers body and find a key in his pouch. Key Acquired!"
    soldier _ = NothingHappened

    sword (PredefAction Take) = SomethingHappened "Sword Acquired!"
    sword (PredefAction Look) = SomethingHappened "A standard longsword, still suprisingly sharp."
    sword _ = NothingHappened

    shield (PredefAction Take) = SomethingHappened "Shield Acquired!"
    shield (PredefAction Look) = SomethingHappened "A standard shield, still still very sturdy."
    shield _ = NothingHappened

    barrel (PredefAction Look) = SomethingHappened "The barrel is small and a key is required to open the barrel. I wonder what could be inside..."
    barrel (PredefAction Attack) = SomethingHappened "You break the barrel open, Health Potion Acquired!"
    barrel (PredefAction UseKey) = SomethingHappened "You use the key to open the barrel, Health Potion Acquired!"
    barrel _ = NothingHappened

    torches (StringAction "light") = Move 1 "You light all the torches on the surrounding walls, suddenly the red orb above the door turns green and the door slides open" 
    torches _ = NothingHappened

-- Room 1 = Hall
-- Acceses to all other rooms cut off except to North room
room1 = Room
  "As you cautiously enter the hall, there is a  red carpet leading to a large chest in the middle of the hall with three locks attached, the hall is lit by the sky with cracks in the ceiling. There a three other doors one to North, one to the South and one to the East."
  (Map.fromList $ [
    ("", Object room),
    ("chest", Object chest)
  ])
  where
    room (PredefAction Look) = SomethingHappened "A Large hall with an obsidian chest in the centre, with three door to the North, South and East. The North door has a large green orb above it whilst the other doors have a large red orb."
    room (PredefAction North) = Move 2 "You push the door open and enter a mysterious room"
    room (PredefAction South) = SomethingHappened "You try to open the door but it won't budge."
    room (PredefAction East) = SomethingHappened "You try to open the door but it won't budge."
    room (PredefAction West) = SomethingHappened "The door is blocked with a mysterious invisible field, seem like you have been trapped in the hall."
    room _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large obsidian chest with three locks, it must contain something valuable"
    chest (PredefAction Open) = SomethingHappened "3 Keys are required to open this lock, perhaps I should explore the dungeon more"
    chest _ = NothingHappened

-- Room 2 = 1st Challenge room
-- Ignite the correct torches to acquire the Key
room2 = Room
  "You enter a room with 5 unlit torches on the North wall and the door suddenly locks behind you changing the colour of the orb above to red."
    (Map.fromList $ [
      ("", Object room),
      ("torches", Object torches),
      ("ignite", Object ignite),
      ("key", Object key)
  ])
  where
    room (PredefAction Look) = SomethingHappened " There are strange patterns on the wall and little rodents scurry about, and on the ceiling a combination of numbers chiselled in 1,3,5."
    room _ = NothingHappened

    torches (PredefAction Look) = SomethingHappened "5 unlit torches across the north wall, perhaps it has something to do with the numbers chiselled above."
    torches _ = NothingHappened

    ignite (StringAction "2,4") = SomethingHappened "The two torches remain lit and a key emerges from the ground. The orb above the door turns green"
    ignite _ = IncorrectCombo

    key (PredefAction Take) = Move 3 "You take the key and return to the hall."
    key _ = IncorrectCombo

-- Room 1a = Second stage of the hall
-- Differences = Updated respones to opening the chest, colour of orbs have changed and doors are blocked with a field.
room1a = Room
  "As you return enter the hall the orb above the north door turns purple, the orb above the south hall has turned green but the orb on the east door is still red."
  (Map.fromList $ [
    ("", Object room),
    ("chest", Object chest)
  ])
  where
    room (PredefAction North) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room (PredefAction South) = Move 4 "You push the door open and enter a mysterious room"
    room (PredefAction East) = SomethingHappened "You try to open the door but it won't budge."
    room (PredefAction West) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large obsidian chest with three locks, it must contain something valuable"
    chest (PredefAction Open) = SomethingHappened "3 Keys are required to open this lock, I only have 1 ,perhaps I should explore the dungeon more"
    chest _ = NothingHappened

-- Room 3 = Challenge room 2
-- Defeat the monster to access the chest
-- Specefic strategy required.
room3 = Room
  "As you enter the room to the south, there lays yet another chest on south wall. There is a narrow passage surrounded by spikes which leads to the chest however there is a monster waiting in the middle of the passage. It fires a rock towards you."
    (Map.fromList $ [
    ("", Object room),
    ("shield", Object shield),
    ("sword", Object sword),
    ("potion", Object potion),
    ("chest" , Object chest)


  ])
  where
    room (PredefAction Look) = SomethingHappened "The only exit is now locked as the orb above the door has turned red. Theres a monster blocking your path to the chest and firing rocks repeatedly. If only there was a way to return fire... "
    room _ = NothingHappened


    shield (PredefAction Block) = SomethingHappened "You block the attack with your shield but the monster fires another shot"
    shield (PredefAction Parry) = Move 5 "You parry the attack with your shield and the rock is deflected back at the monster, the monster is now defeated."
    shield _ = NothingHappened

    sword (PredefAction Attack) = Move 6 "You try to attack the monster, but you are hit by the rock fired at you. You are critically injured."
    sword _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large silver chest lays behind the monster, holding the second key to the obsidian chest."
    chest (PredefAction Open) = PlayerDeath "You attempt to run straight to the chest but you immediately get hit by the monsters attack, you lose balance and fall in to the spikes."
    chest _ = NothingHappened

    potion (PredefAction Take) = SomethingHappened "Health is already full."
    potion _ = NothingHappened

--Room 3a = Challenge room 2 completed
-- Monster defeated
room3a = Room

    "You walk across the accross the narrow passage to access the chest."
      (Map.fromList $ [
      ("", Object room),
      ("chest", Object chest)
    ])
    where
      room (PredefAction Look) = SomethingHappened "A silver chest is infront of you, be careful not to slip or you might fall in the spikes."
      room _ = NothingHappened

      chest (PredefAction Look) = SomethingHappened "A large silver chest"
      chest (PredefAction Open) = Move 7 "You open the chest and find another key for the obsidian chest."
      chest _ = NothingHappened

--Room 3a = Challenge room 2, player damaged
-- Potion use available, Player can also die now.
room3b = Room

    "You can still move, the monster is still blocking your path"
      (Map.fromList $ [
      ("", Object room),
      ("chest", Object chest),
      ("shield", Object shield),
      ("sword", Object sword),
      ("potion", Object potion)
    ])
    where
      room (PredefAction Look) = SomethingHappened "The monster is still firing rocks, maybe if I parry with my shield it can be defeated."
      room _ = NothingHappened

      shield (PredefAction Block) = SomethingHappened "You block the attack with your shield but the monster fires another shot"
      shield (PredefAction Parry) = Move 5 "You parry the attack with your shield and the rock is deflected back at the monster, the monster is now defeated."
      shield _ = NothingHappened

      sword (PredefAction Attack) = PlayerDeath "You attempt to attack the monster again, but you are immediately hit by the monsters attack which shatters you."
      sword _ = NothingHappened


      chest (PredefAction Look) = SomethingHappened "A large silver chest lays behind the monster, holding the second key to the obsidian chest."
      chest (PredefAction Open) = PlayerDeath "You attempt to run straight to the chest but you immediately get hit by the monsters attack, you lose balance and fall in to the spikes."
      chest _ = NothingHappened

      potion (PredefAction Take) = SomethingHappened "You take a health potion. Your Health is now restored."
      potion _ = NothingHappened

-- Room 1b = Hall update 3
-- Updated responses and doors available
room1b = Room
  "As you return enter the hall the orb above the South door turns purple and the orb above the East door has turned green."
  (Map.fromList $ [
    ("", Object room),
    ("chest", Object chest)
  ])
  where
    room (PredefAction North) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room (PredefAction South) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room (PredefAction East) = Move 8 "You push the door open and enter a mysterious room"
    room (PredefAction West) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large obsidian chest with three locks, it must contain something valuable"
    chest (PredefAction Open) = SomethingHappened "3 Keys are required to open this lock, I only have 2,perhaps I should explore the dungeon more..."
    chest _ = NothingHappened

-- Room 4 = Third Challenge Room
-- Must defeat stronger monster to get to the chest.
-- Specefic strategy required.
room4 = Room
  "As you enter the east room, there lays the final chest, as you step forward to open the chest, a monster falls from the sky and begins to attack. The monster strikes with it claws."
    (Map.fromList $ [
    ("", Object room),
    ("shield", Object shield),
    ("sword", Object sword),
    ("potion", Object potion),
    ("chest" , Object chest)


  ])
  where
    room (PredefAction Look) = SomethingHappened "The only exit is now locked as the orb above the door has turned red. Theres a monster blocking your path to the chest and is about to strike you with its razor sharp claws."
    room _ = NothingHappened


    shield (PredefAction Block) = SomethingHappened "You block the attack with your shield, the monster jumps back and prepares to strike again"
    shield (PredefAction Parry) = Move 9 "You parry the attack with your shield and the monster is now vulnerable"
    shield _ = NothingHappened

    sword (PredefAction Attack) = PlayerDeath "You swing your sword at the monster and it lands but you suffer a fatal blow as a result."
    sword _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large Gold chest lays behind the monster, holding the final key to the obsidian chest."
    chest (PredefAction Open) = PlayerDeath "You attempt to run straight to the chest but you immediately get hit by the monsters attack and die as result."
    chest _ = NothingHappened

    potion (PredefAction Take) = SomethingHappened "Health is already full."
    potion _ = NothingHappened

-- Room 4a = Third Challenge Room, monster attack is blocked and now vulenrable
room4a = Room
  "The monsters guard is wide open!"
    (Map.fromList $ [
    ("", Object room),
    ("shield", Object shield),
    ("sword", Object sword),
    ("potion", Object potion),
    ("chest" , Object chest)


  ])
  where
    room (PredefAction Look) = SomethingHappened "The monster is off balance and open to an attack."
    room _ = NothingHappened


    shield _ = NothingHappened

    sword (PredefAction Attack) = Move 10 "You swing your sword at the monster and it lands but is still able to move!"
    sword _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large Gold chest lays behind the monster, holding the final key to the obsidian chest."
    chest (PredefAction Open) = PlayerDeath "You attempt to run straight to the chest whilst the monster is off balance but it manages to catch you with a fatal strike. So close yet so far..."
    chest _ = NothingHappened

    potion (PredefAction Take) = SomethingHappened "Health is already full."
    potion _ = NothingHappened

-- Room 4a = Third Challenge Room, monster damaged from player attack

room4b = Room
  "You managed to land a critical strike against the monster, it still stands and launches another strike in desparation!"
    (Map.fromList $ [
    ("", Object room),
    ("shield", Object shield),
    ("sword", Object sword),
    ("potion", Object potion),
    ("chest" , Object chest)


  ])
  where
    room (PredefAction Look) = SomethingHappened "The monster is wounded...but not going down without a fight!"
    room _ = NothingHappened


    shield (PredefAction Block) = SomethingHappened "You block the attack with your shield, the monster jumps back and prepares to strike again"
    shield (PredefAction Parry) = Move 11  "You parry the attack with your shield and the monster is now vulnerable"
    shield _ = NothingHappened

    sword (PredefAction Attack) = PlayerDeath "You swing your sword at the monster and it lands but you suffer a fatal blow as a result."
    sword _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large Gold chest lays behind the monster, holding the final key to the obsidian chest."
    chest (PredefAction Open) = PlayerDeath "You attempt to run straight to the chest but you immediately get hit by the monsters attack and die as result."
    chest _ = NothingHappened

    potion (PredefAction Take) = SomethingHappened "Health is already full."
    potion _ = NothingHappened


-- Room 4a = Third Challenge Room, monster attack is blocked and now vulenrable to be killed

room4c = Room
  "The monsters guard is wide open! Time to land the final strike!"
    (Map.fromList $ [
    ("", Object room),
    ("shield", Object shield),
    ("sword", Object sword),
    ("potion", Object potion),
    ("chest" , Object chest)
  
  ])
  where
    room (PredefAction Look) = SomethingHappened "The monster is wide open to an attack."
    room _ = NothingHappened
  
    shield (PredefAction Block) = Move 10 "You raise your shield in fear and the monster manages to recover!"
    shield _ = NothingHappened
  
    sword (PredefAction Attack) = Move 12 "You land the final strike on the monster, it falls and shatters into dust!"
    sword _ = NothingHappened
  
    chest (PredefAction Look) = SomethingHappened "A large Gold chest lays behind the monster, holding the final key to the obsidian chest."
    chest (PredefAction Open) = PlayerDeath "You attempt to run straight to the chest but you immediately get hit by the monsters attack and die as result."
    chest _ = NothingHappened
  
    potion (PredefAction Take) = SomethingHappened "Health is already full."
    potion _ = NothingHappened

-- Room 4a = Third Challenge Room, Completed, player can now access chest and return to the hall

room4d = Room
  "The monster has been slain and the golden chest is now in reach."
    (Map.fromList $ [
    ("", Object room),
    ("potion", Object potion),
    ("chest" , Object chest)
  
  ])
  where
    room (PredefAction Look) = SomethingHappened "The monster is dead. The golden chest on the far side of the room is in reach."
    room _ = NothingHappened
  
  
    chest (PredefAction Look) = SomethingHappened "A large Gold chest holding the final key to the obsidian chest."
    chest (PredefAction Open) = Move 13 "You open the final chest. Final Key Acquired!"
    chest _ = NothingHappened
  
    potion (PredefAction Take) = SomethingHappened "Health is already full."
    potion _ = NothingHappened

-- Room 1c = Hall Update 4
-- Game completed after chest has been opened, cannot access any doors
room1c = Room
  "As you return enter the hall the orbs above all doors have turned purple."
  (Map.fromList $ [
    ("", Object room),
    ("chest", Object chest)
  ])
  where
    room (PredefAction Look) = SomethingHappened "All the doors are blocked by a mysterious invisible field, perhaps opening the chest will open an exit."
    room (PredefAction North) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room (PredefAction South) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room (PredefAction East) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room (PredefAction West) = SomethingHappened "The door is blocked with a mysterious invisible field"
    room _ = NothingHappened

    chest (PredefAction Look) = SomethingHappened "A large obsidian chest with three locks, it must contain something valuable"
    chest (PredefAction Open) = GameWon "You opened the obsidian chest...and claimed the ultimate treasure; Programming in Haskell by Graham Hutton, Now you can become a haskell master..."
    chest _ = NothingHappened



