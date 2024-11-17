/* locations.pl */
:- multifile i_am_at/1, at/2, holding/1, forest_cave_ending_killed/0, forest_cave_ending_weakened/0.

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
path(hill_church, w, hill_church_second_floor).

path(hill_church_second_floor, e, hill_church).

path(forest_cave, s, hill_church).
path(forest_cave, n, ending_cave).

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

/* Train Station */

describe(train_station) :-
    i_am_at(train_station),
    write('You are at the train station, where your adventure started.'), nl,
    write('The clock points at 3:15 am and never moves.'), nl,
    write('The timetable is written in some out-of-this-world, unintelligible language.'), nl,
    write('The only person present at the station is the caretaker.'), nl,
    write('To the west, you can see the parking area adjacent to the station.'), nl,
    write('To the east, there is a train which you can use to escape from city.'), nl,
    (holding(amulet) ->
        write('The caretaker looks at you with suspicion as you hold the strange amulet. Something in the air shifts.'), nl
    ; true),
    (holding(harnas) ->
        write('You notice that caretaker is thirsty and there is no water nearby.'), nl
    ; true).

/* Parking */

describe(parking) :-
    write('You are in a deserted parking lot near the train station. The ground is littered'), nl,
    write('with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry.'), nl,
    write('To the north, you can see a homeless man sitting on a bench.'), nl,
    write('To the south is the main street,'), nl,
    write('and the train station is to the east.'), nl,
    (holding(car_keys) -> write('You have the car keys so you can try to open the abandoned car.'), nl,
        write('Type in enter(car) in order to get into the vehicle.'), nl ; true).

describe(car) :-
    write('You sit inside the car, but it refuses to start.'), nl,
    write('Type in exit(car) in order to get out of the vehicle.'), nl,
    (at(key, car) -> write('You noticed an old key lying on the passenger seat.'), nl ; true).

/* Bench with Homeless Man */

describe(homeless_bench) :-
    write('You find yourself near a bench occupied by a homeless man, muttering under his breath.'), nl,
    write('He warns of the "shadows that follow at night" and clutches an old bottle with'), nl,
    write('a strange symbol scratched into it. He might know more if you listen closely.'), nl,
    write('You notice that he cannot breathe properly, he probably ran out of cigarettes.'), nl,
    write('You can interact with him by interact(homeless)'), nl,
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
    write('and faded signs give the area a ghostly feel.'), nl,
    write('To the west is the old town,'), nl,
    write('while the river tracks are to the east,'), nl,
    write('and the parking lot to the north.'), nl.

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
    ( at(amulet, hotel_basement) ->
        write('Among the piles of old crates and broken furniture, something stands out -'), nl,
        write('a strange amulet positioned in the center of the room.'), nl
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

describe(police_station) :-
    nl,
    write('You enter the police station. The lights flicker, casting eerie shadows on the walls.'), nl,
    write('To the left, you see a dusty counter with an old safe behind it. '), nl,
    write('The smell of cheap liquor and stale cigarettes fills the air.'), nl,
    write('A drunkard, with tangled hair and a worn-out jacket, stares at you with a glazed look.'), nl,
    write('He seems to be clutching an old bottle. Perhaps he knows something useful?'), nl,
    write('You can interact with him by interact(drunkard)'), nl,
    write('You can enter the code to the safe by open_safe(Code).'), nl,
    write('You can go south to return to the old town.'), nl,
    nl.

describe(library) :-
    nl,
    write('You enter the library. The room is quiet, filled with towering shelves of old, dusty books.'), nl,
    ( at(newspaper, library) ->
        write('In the center, there is a small table with a newspaper lying on it.'), nl
    ;
        true
    ),
    write('To the south, you see a door leading to the archive room.'), nl,
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

/* Hill Church Description */
describe(hill_church) :-
    i_am_at(hill_church),
    write('You stand before an old, abandoned church on the hill.'), nl,
    write('The dark interior and the smell of incense remind you of ancient rituals.'), nl,
    write('The priest, the last witness of the former life in the city, looks at you with an expression of concern.'), nl,
    (holding(amulet) ->
        write('The priest notices the amulet in your hand and warns you: "That is the symbol of their cult; do not approach them with it."'), nl,
        write('He continues: "I can''t tell you more, just be careful"'), nl,
        write('He points at the old staircase, at the west side of the church, that leads to second floor of the church'), nl
    ; true),
    write('The path to the forest is to the north.'), nl.

describe(hill_church_second_floor) :-
    i_am_at(hill_church_second_floor),
    write('You are now at the second floor of the church.'), nl,
    write('You see some kind of weird numbers that seem out of order at the wall'), nl,
    write('4 6 1 2 6 7 3 4 1 5 6 2 7 3 5 7 3 2 5 3 6 4 3 6 7 2 '), nl,
    write('You notice also a sentence:"That''s a code to the truth of this mystery."'), nl,
    write('To go back to first floor go east'), nl.

/* Forest Cave */
describe(forest_cave) :-
    i_am_at(forest_cave),
    write('You enter a dark cave hidden deep in the forest. In the center of the cave stands an altar with a strange symbol.'), nl,
    write('The symbol looks familiar, it might be an ancient artifact sought by the archaeologists.'), nl,
    write('Here you find evidence that the cult still exists and conducts its rituals here.'), nl,
    write('You feel that this place may be key to solving the mystery of the archaeologists’ disappearance.'), nl,
    write('You see far far to the north of this huge cave a weird doors that must lead to something.'), nl,
    write('The path back leads south, returning to the church.'), nl.

describe(ending_cave) :-
    i_am_at(ending_cave),
    (holding(engraved_ring)->
        forest_cave_ending_weakened;
        forest_cave_ending_killed
    ).
