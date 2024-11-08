/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* Starting point in game */
i_am_at(train_station).

/* Locations to go to */
path(train_station, w, parking).

path(parking, n, homeless_bench).
path(parking, s, main_street).
path(parking, e, train_station).

path(homeless_bench, s, parking).

path(main_street, w, old_town).
path(main_street, n, parking).
path(main_street, e, river_tracks).

path(river_tracks, w, main_street).

path(old_town, n, police_station).
path(old_town, s, hotel_lobby).
path(old_town, w, library).
path(old_town, e, main_street).

path(hotel_lobby, n, old_town).
path(hotel_lobby, e, hotel_toilet).
path(hotel_lobby, s, hotel_corridor).
path(hotel_lobby, w, hotel_basement).

path(hotel_basement, e, hotel_lobby).

path(hotel_toilet, w, hotel_lobby).

path(hotel_corridor, n, hotel_lobby).
path(hotel_corridor, w, hotel_room).

path(hotel_room, e, hotel_corridor).

path(secret_room, n, hotel_lobby).

path(police_station, s, old_town).

path(library, e, old_town).
path(library, w, hill_church).

path(hill_church, n, forest_cave).
path(hill_church, e, library).

path(forest_cave, s, hill_church).
path(forest_cave, n, ending).

at(thing, someplace).

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


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


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(old_town) :-
        nl,
        write('You find yourself in the heart of Old Town,'), nl,
        write('a desolate square filled with abandoned shops and crumbling facades.'), nl,
        write('Dust and debris cover the cobblestone streets, and a faint echo of past lives lingers in the air.'), nl,
        write('Faded graffiti and strange symbols are scrawled across the walls, remnants of a forgotten era.'),
        nl,
        write('In the north there is a police station,'), nl,
        write('in the east you can see the main street.'), nl,
        write('There is an old hotel to the south'), nl,
        write('and a library to the west.'),
        nl.

