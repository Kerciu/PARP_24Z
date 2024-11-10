/* game.pl */
:- dynamic i_am_at/1, holding/1, gave_cigarettes/1, gave_harnas/1.

:- retractall(i_am_at(_)), retractall(holding(_)), retractall(gave_cigarettes(_)), retractall(gave_harnas(_)).

/* Include other files */
:- [locations, objects, characters, actions].

/* Starting point in game */
i_am_at(train_station).

/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.               -- to start the game.'), nl,
        write('n.  s.  e.  w.       -- to go in that direction.'), nl,
        write('take(Object).        -- to pick up an object.'), nl,
        write('drop(Object).        -- to put down an object.'), nl,
        write('look.                -- to look around you again.'), nl,
        write('interact(Character). -- to interact with characters.'), nl,
        write('give(Character, Object) -- to give object to character.'), nl,
        write('inventory. -- to check inventory contents.'), nl,
        write('instructions.        -- to see this message again.'), nl,
        write('halt.                -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.
