/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

<<<<<<< HEAD
/* Initial player's location */
i_am_at(train_station).

/* Paths on the map */
path(train_station, n, main_street).
path(main_street, n, old_hotel).
path(main_street, w, police_station).
path(main_street, e, library),
path(old_hotel, e, hill_castle).
path(hill_castle, n, forest_cave).
=======
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
path(old_town, s, old_hotel).
path(old_town, w, library).
path(old_town, e, main_street).
path(old_hotel, s, secret_room).
path(old_hotel, n, old_town).
path(secret_room, n, old_hotel).
path(police_station, w, library).
path(police_station, s, old_town).
path(library, s, old_town).
path(library, e, police_station).
path(library, w, hill_church).
path(hill_church, n, forest_cave).
path(hill_church, e, library).
path(forest_cave, s, hill_church).
path(forest_cave, n, ending).
>>>>>>> ceb33b22439058b805a6f24468e7f43555e8024b

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

describe(someplace) :- write('You are someplace.'), nl.

