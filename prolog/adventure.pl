/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1, gave_cigarettes/1, gave_harnas/1, interact/2.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(gave_cigarettes(_)), retractall(gave_harnas(_)).

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

at(cigarettes, river_tracks).
at(car_keys, train_station).
at(harnas, homeless_bench).
at(amulet, car).

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

/* Holding items rules */

holding :-
    holding(X),
    write('You are holding: '),
    write(X), nl,
    !.

holding :-
    write('You are not holding anything.'), nl.


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
        \+ member(X, [amulet, harnas, car_keys]),
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
        write('start.               -- to start the game.'), nl,
        write('n.  s.  e.  w.       -- to go in that direction.'), nl,
        write('take(Object).        -- to pick up an object.'), nl,
        write('drop(Object).        -- to put down an object.'), nl,
        write('look.                -- to look around you again.'), nl,
        write('interact(Character). -- to interact with characters.'), nl,
        write('instructions.        -- to see this message again.'), nl,
        write('halt.                -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.

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

check(_) :- nl.

/* Interaction with characters */

give(homeless, cigarettes) :-
    i_am_at(homeless_bench),
    holding(cigarettes),
    \+ gave_cigarettes(homeless_bench),
    write('You give the pack of cigarettes to the homeless man. He takes them eagerly and thanks you.'), nl,
    write('He hands you a cold can of Harnas beer in return.'), nl,
    retract(holding(cigarettes)),
    assert(holding(harnas)),
    assert(gave_cigarettes(homeless_bench)).

give(homeless, harnas) :-
    i_am_at(homeless_bench),
    holding(harnas),
    write('The homeless man rejects your offer to give him Harnas beer.'), nl,
    write('He already has plenty of those.'), nl.

interact(homeless) :-
    i_am_at(homeless_bench),
    write('The homeless man seems to be uninterested for now.'), nl.

interact(homeless) :-
    i_am_at(homeless_bench),
    holding(amulet),
    write('The homeless man looks at you with wide eyes. "You hold that cursed thing," he says, '), nl,
    write('his voice shaking. "I feel something watching... beware."'), nl,
    !.

give(caretaker, harnas) :-
    i_am_at(train_station),
    holding(harnas),
    \+ gave_harnas(train_station),
    write('You offer the Harnas beer to the caretaker. She takes it gratefully and takes a swig.'), nl,
    write('"Ahh, that takes me back," she sighs and tells you more about her story with the homeless man.'), nl,
    write('Grateful, she hands you the car keys.'), nl,
    retract(holding(harnas)),
    assert(holding(car_keys)),
    assert(gave_harnas(train_station)).

give(caretaker, cigarettes) :-
    i_am_at(train_station),
    holding(cigarettes),
    write('The caretaker rejects your offer to give her cigarettes.'), nl.

interact(caretaker) :-
    i_am_at(train_station),
    holding(amulet),
    write('The caretaker looks at you uneasily as you hold the strange amulet. "What is that you have there?" she asks, '), nl,
    write('her eyes narrowing. "You should be careful where you go with that."'), nl,
    !.

interact(caretaker) :-
    i_am_at(train_station),
    write('The caretaker seems more distant and uninterested for now.'), nl.

/* Take rules for the items */

take(harnas) :-
    holding(harnas),
    write('You already have the Harnas beer that the homeless man gave you.'), nl,
    !.

take(harnas) :-
    \+ gave_cigarettes(homeless_bench),
    write('The homeless man has not given you the Harnas beer yet. You need to give him something first.'), nl,
    !.

take(harnas) :-
    i_am_at(homeless_bench),
    at(harnas, homeless_bench),
    write('You take the cold can of Harnas beer from the homeless man after giving him cigarettes.'), nl,
    retract(at(harnas, homeless_bench)),
    assert(holding(harnas)),
    !.


/* Rules for entering and exiting the car */

enter(car) :-
    holding(car_keys),
    i_am_at(parking),
    write('You unlock the car with the keys and sit inside, but it doesn''t start.'), nl,
    retract(i_am_at(parking)),
    assert(i_am_at(car)),
    retract(holding(car_keys)),
    look.

exit(car) :-
    i_am_at(car),
    write('You get out of the car and return to the parking lot.'), nl,
    retract(i_am_at(car)),
    assert(i_am_at(parking)).


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
    (holding(harnas), \+ gave_harnas(train_station) ->
        give_harnas(caretaker)
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
    (\+ gave_cigarettes(homeless_bench) ->
        'You notice that he cannot breathe properly; he probably ran out of cigarettes.'
    ; true),
    write(), nl,
    write('The parking area lies to the south.'), nl.

describe(homeless_bench) :-
    i_am_at(homeless_bench),
    (holding(cigarettes), \+ gave_cigarettes(homeless_bench) ->
        give_cigarettes(homeless)
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
