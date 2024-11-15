/* locations.pl */
:- dynamic i_am_at/1, at/2, holding/1.


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
path(library, s, archive).
path(library, w, hill_church).

path(archive, n, library).

path(hill_church, n, forest_cave).
path(hill_church, e, library).

path(forest_cave, s, hill_church).
path(forest_cave, n, ending).

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
        write('You can go north to return to Old Town.'), nl,
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
        ( at(notes, hotel_basement) ->
                write('You notice a set of old, crumbling notes scattered across a dusty table.'), nl
        ;
                true
        ),
        nl,
        write('You can go east to return to the hotel lobby.'), nl,
        nl.

describe(library) :-
        nl,
        write('You enter the library. The room is quiet, filled with towering shelves of old, dusty books.'), nl,
        ( at(newspaper, library) ->
                write('In the center, there is a small table with a newspaper lying on it.'), nl
        ;
                true
        ),
        write('To the south, you see a door leading to the archive room, but it appears to be locked.'), nl,
        nl.

describe(archive) :-
        nl,
        write('You enter the archive room. It is small and cramped, filled with stacks of old papers and documents.'), nl,
        write('Most of the documents are unreadable due to age.'), nl,
        ( at(green_fuse, archive) ->
                write('However you notice a bright green fuse lying on a nearby shelf.'), nl
        ;
                true
        ),
        nl.
