/* locations.pl */

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
    (\+ gave_harnas(train_station) ->
        (holding(harnas) ->
            write('You notice that caretaker is thirsty and there is no water nearby.'), nl;
            true)
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
    (\+ gave_cigarettes(homeless_bench) ->
        (holding(cigarettes) ->
            write('You notice that you can help the homeless man with a cigarette.'), nl;
            true)
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
