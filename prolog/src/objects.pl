/* These rules describe the objects in the game. */
:- dynamic i_am_at/1, holding/1, at/2.

/* Inventory rules */

inventory :-
    \+ (holding(_)) ->
        write('You are not holding anything.'), nl
    ; (
        write('You are currently holding the following items: '), nl,
        (holding(X), write('- '), write(X), nl, fail ; true)
    ).

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


/* Locations of objects */
at(cigarettes, river_tracks).
at(amulet, car).
at(diary, hotel_room).
at(red_fuse, hotel_toilet).
at(blue_fuse, hotel_room).
at(ancient_rune, hotel_basement).
at(notes, hotel_basement).
at(newspaper, library).
at(green_fuse, archive).
at(key, library).

/* These rules describe objects that can be picked up. */

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

check(notes) :-
    nl,
    write('The notes are brittle and covered in dust, with several pages missing or torn.'), nl,
    nl,
    write('"We found it — the ruin from the old texts.'), nl,
    write('It is said this ruin is the key to something ancient, hidden deep within the caves.'), nl,
    write('We must be cautious. The energy here is unstable, and there are warnings in the old language about disturbing what lies beyond."'), nl,
    nl.

check(ancient_rune) :-
    nl,
    write('The rune is etched with symbols that seem to shift and change as you look at them.'), nl,
    write('It feels warm to the touch, as if it is alive.'), nl,
    nl.

check(newspaper) :-
    nl,
    write('You start reading it. The headline catches your eye:'), nl,
    write('"Team of Archaeologists Arrives to Uncover the Secrets of the Ancient Ruins in Town."'), nl,
    nl,
    write('The article mentions several archaeologists by name:'), nl,
    write('- Sarah Miller (born 1952)'), nl,
    write('- John Roberts (born 1968)'), nl,
    write('- Michael Turner (born 1974)'), nl,
    write('- Emma Carter (born 1980)'), nl,
    nl.

check(_) :- nl.