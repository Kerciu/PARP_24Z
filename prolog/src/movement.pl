/* movement.pl */
:- multifile i_am_at/1, at/2, holding/1, path/3, door_unlocked/1, interact/2, describe/1, escape_city_ending/0, hill_church_ending_escape/0.

/* State of rooms */
door_unlocked(hotel_room) :- fail.
door_unlocked(hotel_basement) :- fail.
door_unlocked(archive) :- fail.

/* This rule tells how to look around you. */
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

/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* These rules tells how to move in a given direction. */

/* Conditional rules for moving */
go(e) :-
        i_am_at(train_station),
        escape_city_ending.
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

go(n) :-
    i_am_at(hill_church_second_floor),
    holding(weird_box),
    hill_church_ending_escape.

go(n) :-
    i_am_at(forest_cave),
    \+ holding(amulet),
    write('A strange force seems to block your path. The entrance won’t budge.'), nl,
    write('It feels like something is missing, something that could unlock the cave\'s secrets.'), nl,
    !.

go(n) :-
    i_am_at(forest_cave),
    holding(amulet),
    write('The amulet in your possession glows faintly, and you feel an ancient force giving way.'), nl,
    write('The entrance opens, allowing you to pass into the unknown.'), nl,
    retract(i_am_at(forest_cave)),
    assert(i_am_at(ending_cave)),
    !, look.

go(s) :-
    i_am_at(library),
    \+ door_unlocked(archive),
        ( holding(key) ->
            write('You open the door with the key.'), nl,
            retractall(holding(key)),
            assert(door_unlocked(archive)),
            go(s)
        ;
            write('The door is locked.'), nl,
            write('You need a key to unlock it.'), nl
        ).

/* General rules for moving */
go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(_) :-
    write('You can''t go that way.').

/* Rules for entering and exiting the car */

enter(car) :-
    i_am_at(parking),
    holding(car_keys),
    write('You unlock the car with the keys.'), nl,
    retract(i_am_at(parking)),
    assert(i_am_at(car)),
    look.

enter(car) :-
    i_am_at(parking),
    \+ holding(car_keys),
    write('You need the car keys to enter the car.'), nl.

enter(car) :-
    i_am_at(car),
    write('You are already inside the car.'), nl.

enter(car) :-
    write('There is no car here.'), nl.

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
        retractall(holding(red_fuse)),
        retractall(holding(blue_fuse)),
        retractall(holding(green_fuse)),
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

/* This rule tells how to unlock the safe. */

open_safe(Code) :-
    /* The code can be found as an easter egg in the leaf code drunkard gave you */
    i_am_at(police_station),
    number(Code),
    Code =:= 2137,
    assert(holding(engraved_ring)),
    nl,
    write('The safe opens, revealing an engraved ring inside.'), nl,
    write('You take the engraved ring.'), nl,
    !.

open_safe(_) :-
    write('Wrong code.'), nl,
    !, look.
