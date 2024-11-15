/* main.pl */
:- dynamic i_am_at/1, at/2, holding/1, door_unlocked/1, interact/2.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(door_unlocked(_)).

/* Load all the files */
:- [src/locations].
:- [src/objects].
:- [src/interactions].
:- [src/movement].
:- [src/endings].

i_am_at(train_station).

start :-
        instructions,
        start_timer,
        look.

/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.           -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('interact(Character).     -- to interact with characters.'), nl,
        write('give(Character, Object)  -- to give object to character.'), nl,
        write('inventory.               -- to check inventory contents.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        nl.


/* This rule tells how to die. */

die :-
        finish.


    /* Under UNIX, the "halt." command quits Prolog but does not
    remove the output window. On a PC, however, the window
    disappears before the final output can be seen. Hence this
    routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.

