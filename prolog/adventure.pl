/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1, door_unlocked/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(door_unlocked(_)).

/* Starting point in game */
i_am_at(hotel_basement).

/* State of rooms */
door_unlocked(hotel_room) :- fail.
door_unlocked(hotel_basement) :- fail.

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

/* Locations of objects */
at(diary, hotel_room).
at(red_fuse, hotel_lobby).
at(blue_fuse, hotel_lobby).
at(green_fuse, hotel_lobby).
at(ancient_rune, hotel_basement).
at(basement_notes, hotel_basement).

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
        write('You picked up the '), write(X), write('.'),
        check(X),
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
        write('You dropped the '), write(X), write('.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* These rules tells how to move in a given direction. */

/* Conditional rules for moving */
go(w) :-
        i_am_at(hotel_corridor),
        \+ door_unlocked(hotel_room),
                write('Door is locked.'), nl,
                write('Next to the door, there is a sign that reads: "Michael Turner".'), nl,
                write('You have to enter 4 digit code to unlock the door.'), nl,
                read(Code),
                try_unlock_hotel_room(Code),
                !.

go(w) :-
        i_am_at(hotel_lobby),
        \+ door_unlocked(hotel_basement),
                try_unlock_hotel_basement,
                !.

/* General rules for moving */
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
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is '), write(X), write(' here.'), nl,
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

/* This rule tells how to unlock the hotel room door. */
try_unlock_hotel_room(Code) :-
        /* The code can be found in a different location as year of birth of Michael Turner */
        Code = 1974,
        retractall(door_unlocked(hotel_room)),
        assert(door_unlocked(hotel_room)),
        nl,
        write('You have unlocked the door.'), nl,
        go(w),
        !.

try_unlock_hotel_room(_) :-
        write('Wrong code.'), nl,
        !, look.

/* This rule tells how to unlock the hotel basement door. */
try_unlock_hotel_basement :-
        (holding(red_fuse), holding(blue_fuse), holding(green_fuse) ->
                nl,
                write('You put all 3 fuses in the fuse box.'), nl,
                write('The elevator starts working.'), nl,
                retractall(door_unlocked(hotel_basement)),
                assert(door_unlocked(hotel_basement)),
                go(w)
        ;
                nl,
                write('The elevator is not working.'), nl,
                write('However, you notice a fuse box next to the elevator with 3 fuses missing.'), nl,
                nl,
                write('"Maybe if I find the fuses, I can get the elevator working."'), nl,
                look
        ).

/* These rules describe the objects in the game. */
check(diary) :-
        nl,
        write('The diary is yellowed and fragile.'), nl,
        write('Some pages are barely readable, but one section stands out, scribbled with urgency:'), nl,
        nl,
        write('"We have found traces of an entrance near the old oak in the forest.'), nl,
        write('It must be the cave mentioned in the legends... The symbols match.'), nl,
        write('It is said the cave holds more than relics — perhaps a power that should remain undisturbed.'), nl,
        write('We must proceed with caution."'), nl,
        nl.

check(basement_notes) :-
        nl,
        write('The notes are brittle and covered in dust, with several pages missing or torn.'), nl,
        nl,
        write('"We found it — the ruin from the old texts.'), nl,
        write('It is said this ruin is the key to something ancient, hidden deep within the caves.'), nl,
        write('We must be cautious. The energy here is unstable, and there are warnings in the old language about disturbing what lies beyond."'), nl,
        nl.

check(_) :- nl.


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

describe(hotel_lobby) :-
        nl,
        write('You enter the lobby of the hotel.'), nl,
        write('There is glass scattered everywhere, and the old reception desk is covered in papers.'), nl,
        nl,
        write('You can go south to return to Old Town.'), nl,
        write('There is a toilet to the east.'), nl,
        write('To the south, a dark hallway leads further into the hotel.'), nl,
        write('Looking to the west, you can see an elevator.'), nl,
        nl.

describe(hotel_toilet) :-
        nl,
        write('You enter the toilet.'), nl,
        write('The room is dark and damp, and the smell of mold and decay fills the air.'), nl,
        write('The toilet is broken and the sink is covered in grime.'), nl,
        nl,
        write('You can go west to return to the hotel lobby.'), nl,
        nl.

describe(hotel_corridor) :-
        nl,
        write('You enter a dark hallway.'), nl,
        write('The ceiling has collapsed, thus most of the corridor is blocked off by rubble.'), nl,
        nl,
        write('Only one room to the west remains accessible.'), nl,
        write('You can go north to return to the hotel lobby.'), nl,
        nl.

describe(hotel_room) :-
        nl,
        write('You enter the hotel room.'), nl,
        write('The room is dark and dusty, and the bed is covered in old sheets.'), nl,
        write('The closet is empty, and the desk is covered in papers.'), nl,
        nl,
        write('You can go east to return to the corridor.'), nl,
        nl.

describe(hotel_basement) :-
        nl,
        write('The elevator can only go down.'), nl,
        write('It leads to the basement of the hotel.'), nl,
        write('The basement is dark and damp, with a faint, musty odor filling the air.'), nl,
        nl,
        /* Describe objects in the basement only if they are actually there */
        ( at(ancient_rune, hotel_basement) ->
                write('Among the piles of old crates and broken furniture, something stands out -'), nl,
                write('a strange, ancient rune carved from dark stone, positioned in the center of the room.'), nl
        ;
                true
        ),
        ( at(basement_notes, hotel_basement) ->
                write('You notice a set of old, crumbling notes scattered across a dusty table.'), nl
        ;
                true
        ),
        nl.
