
escape_city_ending :-
    (holding(diary) ->
        write('After piecing together some of the fragments of the city''s history you decide to leave.'), nl;
        write('You have''t learned anything yey decided to leave this town.'), nl
    ),
    write('Pathetic isn''t it.'), nl,
    write('Either way city''s shadows grow darker with each discovery, hinting at forces far beyond human understanding.'), nl,
    write('You relized the danger, deciding to abandon the case, leaving the mysteries of the City of Shadows unsolved.'), nl,
    write('That might''ve been a wise decision, who knows?'), nl,
    write('The chilling secrets of this forgotten town remain buried, but you manages to escape with your life haunted by unanswered questions.'),
    finish.

hill_church_ending_excape :-
    write('You''ve discovered everything that was possible to be disctovered.'), nl,
    write('Yet, decided to leave. Might''ve been wise or because of your fear you left this city for the doom.'), nl,
    write('So be it, farewell.'),
    finish.

forest_cave_ending_weakened :-
    write('The weird ring started to glowing giving you the power to destroy all of the cult places.'), nl,
    write('Armed with every piece of the puzzle, the detective confronts the heart of the sect’s power.'), nl,
    write('Having the power of ring you easly destroys the most powerful place of cult.'), nl,
    write('The city, now freed from its dark binds, is left desolate and cursed, but you walk away victorious.'), nl,
    write('The shadows may linger, but the sect’s grip on the City of Shadows is broken, its haunting legacy fading into history.'),
    finish.

forest_cave_ending_killed :-
    write('The cult for long knew that you were on their tails.'), nl,
    write('Yet, as you finally grasp the full horror of the sect''s motives, you are caught in a trap, surrounded by the sect’s followers.'), nl,
    write('Expecting you the ambush you and they easily kill you.'), nl,
    write('Your journey ends here, as they are drawn into the dark embrace of the city, their fate forever tied to its haunting secrets.'),
    finish.