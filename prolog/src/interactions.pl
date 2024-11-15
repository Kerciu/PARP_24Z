/* Interaction with characters */
:- dynamic i_am_at/1, holding/1.

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