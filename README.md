# Interactive fiction inspired by *Alien*
Authors: Julia Czosnek, Kinga Łukiewicz, Małgorzata Grzanka

Implemented *Alien*-inspired  interactive fiction game implemented in Prolog, Haskell, and Smalltalk, showcasing different approaches to logic, functional, and object-oriented programming for the same story.
Project realised as part of Programming Paradigms course at WUT.

## Premise

The game allows the player to personate warrant officer Riley, one the passangers of the Nostromo spaceship sent on a mission to investiagte a potential lifeform on planet *Darconis-26*. But not everything goes according to plan, and when Ripley has to take commmand, she realises there may be another form of danger lurking in the dark corridors.

## Rules

The rules are simple and the game is intuitive. The player interacts with the world by using the commands which they can always look up by using `instructions` command, slightly different depending on the language syntax.
Available commands are:
```
start.               -- to start the game.
go(Room).            -- to enter the room.
take(Object).        -- to pick up an object.
look.                -- to look around the room.
investigate(Person). -- to investigate someone.
crew.                -- to see the list of the crew.
rooms.               -- to see the list of the rooms.
instructions.        -- to see this list again.
stop.                -- to end the game.
```

## Set-up

To set-up the environment on Ubuntu, you just need to install `swi-prolog`, `haskell-platform` and `gnu-smalltalk` by running:
```
$ sudo apt-get update
$ sudo apt-get install -y swi-prolog
$ sudo apt-get install -y ghc
```

For Ubuntu from version 21.10 (amd64) for smalltalk download those packets and install them in console in that order:
1. https://launchpad.net/ubuntu/impish/amd64/libgst7/3.2.5-1.3ubuntu1
2. https://launchpad.net/ubuntu/impish/amd64/gnu-smalltalk-common/3.2.5-1.3ubuntu1
3. https://launchpad.net/ubuntu/impish/amd64/gnu-smalltalk/3.2.5-1.3ubuntu1

Enjoy the game :)