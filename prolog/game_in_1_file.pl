/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1, door_unlocked/1, interact/2.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(door_unlocked(_)).

/* Starting point in game */
i_am_at(hotel_lobby).

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
at(cigarettes, river_tracks).
at(amulet, car).
at(diary, hotel_room).
at(red_fuse, hotel_toilet).
at(blue_fuse, hotel_room).
at(green_fuse, hotel_lobby).
at(ancient_rune, hotel_basement).
at(basement_notes, hotel_basement).

/* These rules describe how to pick up an object. */

take(X) :-
    holding(X),
    write('You are already holding the '), write(X), write('!'), nl,
    !.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    assert(holding(X)),
    write('You have taken the '), write(X), write('.'), nl,
    check(X),
    !, nl.

take(_) :-
    write('I don''t see that here.'), nl.

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
        \+ member(X, [amulet]),
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

/* Inventory rules */

inventory :-
    \+ (holding(_)) ->
        write('You are not holding anything.'), nl
    ; (
        write('You are currently holding the following items: '), nl,
        (holding(X), write('- '), write(X), nl, fail ; true)
    ).

/* Interaction with characters */

give(homeless, cigarettes) :-
    i_am_at(homeless_bench),
    holding(cigarettes),
    write('You give the pack of cigarettes to the homeless man. He takes them eagerly and thanks you.'), nl,
    write('He hands you a cold can of Harnas beer in return.'), nl,
    retract(holding(cigarettes)),
    assert(holding(harnas)).

give(homeless, harnas) :-
    i_am_at(homeless_bench),
    holding(harnas),
    write('The homeless man rejects your offer to give him Harnas beer.'), nl,
    write('He already has plenty of those.'), nl.

give(caretaker, harnas) :-
    i_am_at(train_station),
    holding(harnas),
    write('You offer the Harnas beer to the caretaker. She takes it gratefully and takes a swig.'), nl,
    write('"Ahh, that takes me back," she sighs and tells you more about her story with the homeless man.'), nl,
    write('Grateful, she hands you the car keys.'), nl,
    retract(holding(harnas)),
    assert(holding(car_keys)).

give(caretaker, cigarettes) :-
    i_am_at(train_station),
    holding(cigarettes),
    write('The caretaker rejects your offer to give her cigarettes.'), nl.

give(_, _) :-
    write('You can''t give that to that person.'), nl.

interact(homeless) :-
    i_am_at(homeless_bench),
    holding(cigarettes),
    write('The homeless man seems to be more interested as he coughts intensively.'), nl.

interact(homeless) :-
    i_am_at(homeless_bench),
    holding(amulet),
    write('The homeless man looks at you with wide eyes. "You hold that cursed thing," he says, '), nl,
    write('his voice shaking. "I feel something watching... beware."'), nl,
    !.

interact(homeless) :-
    i_am_at(homeless_bench),
    write('The homeless man seems to be uninterested for now.'), nl.

interact(caretaker) :-
    i_am_at(train_station),
    holding(amulet),
    write('The caretaker looks at you uneasily as you hold the strange amulet. "What is that you have there?" she asks, '), nl,
    write('her eyes narrowing. "You should be careful where you go with that."'), nl,
    !.

interact(caretaker) :-
    i_am_at(train_station),
    write('The caretaker seems more distant and uninterested for now.'), nl.

/* Rules for entering and exiting the car */

enter(car) :-
    holding(car_keys),
    i_am_at(parking),
    write('You unlock the car with the keys and sit inside, but it doesn''t start.'), nl,
    retract(i_am_at(parking)),
    assert(i_am_at(car)),
    look.

exit(car) :-
    i_am_at(car),
    write('You get out of the car and return to the parking lot.'), nl,
    retract(i_am_at(car)),
    assert(i_am_at(parking)).

/* This rule tells how to unlock the hotel room door. */

try_unlock_hotel_room(Code) :-
        /* The code can be found in a different location as year of birth of Michael Turner */
        number(Code),
        Code =:= 1974,
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

check(cigarettes) :-
    holding(cigarettes),
    write('The pack of cigarettes is old, with some of the edges frayed. It might be useful to someone who needs a smoke.'), nl.

check(harnas) :-
    holding(harnas),
    write('A can of Harnas beer, cold and seemingly untouched. It might help you in your interactions with certain characters.'), nl.

check(car_keys) :-
    holding(car_keys),
    write('A set of car keys. They seem to belong to the abandoned car in the parking lot. Maybe you can unlock it.'), nl.

check(amulet) :-
    holding(amulet),
    write('A strange amulet, cold to the touch. It seems to have an otherworldly aura, as if it carries a hidden power.'), nl.

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

/* Train Station */

describe(train_station) :-
    i_am_at(train_station),
    write('You are at the train station, where your adventure started.'), nl,
    write('The clock points at 3:15 am and never moves.'), nl,
    write('The timetable is written in some out-of-this-world, unintelligible language.'), nl,
    write('The only person present at the station is the caretaker, who seems reluctant to chat.'), nl,
    write('To the west, you can see the parking area adjacent to the station.'), nl,
    (holding(amulet) ->
    (write('The caretaker looks at you with suspicion as you hold the strange amulet. Something in the air shifts.'), nl)
    ;
    true
    ).

describe(train_station) :-
    i_am_at(train_station),
    (holding(harnas) ->
            write('You notice that caretaker is thirsty and there is no water nearby.'), nl
    ; true),
    write('The parking area lies to the west.'), nl.

/* Parking */

describe(parking) :-
    write('You are in a deserted parking lot near the train station. The ground is littered'), nl,
    write('with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry.'), nl,
    write('To the north, you can see a homeless man sitting on a bench.'), nl,
    write('To the south is the main street,'), nl,
    write('and the train station is to the east.'), nl,
    (holding(car_keys) -> write('You have the car keys so you can try to open the abandoned car.'), nl ; true).

describe(car) :-
    write('You sit inside the car, but it refuses to start.'), nl,
    (at(amulet, car) -> write('You spotted weird amulet lying on the backseat of the car.'), nl ; true).

/* Bench with Homeless Man */

describe(homeless_bench) :-
    write('You find yourself near a bench occupied by a homeless man, muttering under his breath.'), nl,
    write('He warns of the "shadows that follow at night" and clutches an old bottle with'), nl,
    write('a strange symbol scratched into it. He might know more if you listen closely.'), nl,
    write('You notice that he cannot breathe properly, he probably ran out of cigarettes.'), nl,
    (holding(cigarettes) ->
        write('You notice that you can help the homeless man with a cigarette.'), nl
    ; true),
    write('The parking area lies to the south.'), nl.

/* River Next to the Train Tracks */

describe(river_tracks) :-
    write('You stand by the river tracks. The water flows sluggishly, casting eerie reflections'), nl,
    write('in the moonlight.'), nl,
    write('To the west, you see the main street.'), nl,
    (at(cigarettes, river_tracks) -> write('Near the edge of the river, you notice a pack of cigarettes lying in the grass.'), nl ; true).

/* Main Street */

describe(main_street) :-
    write('You are on the main street, flanked by old, abandoned shops. The cracked windows'), nl,
    write('and faded signs give the area a ghostly feel. Farther down the street, you spot'), nl,
    write('a shifty figure lurking in the shadows.'), nl,
    write('To the west is the old town, while the river tracks are to the east, and the parking lot to the north.'), nl.

describe(old_town) :-
        nl,
        write('You find yourself in the heart of Old Town,'), nl,
        write('a desolate square filled with abandoned shops and crumbling facades.'), nl,
        write('Dust and debris cover the cobblestone streets, and a faint echo of past lives lingers in the air.'), nl,
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
        (at(diary, hotel_room) ->
                write('You notice a diary lying on the desk.'), nl
        ;
                true
        ),
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
        nl,
        write('You can go east to return to the hotel lobby.'), nl,
        nl.
