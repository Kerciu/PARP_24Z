Object subclass: Endings [

    Endings class >> escapeCityEndingText: holdingDiary [
        | text |

        text := ''.

        holdingDiary
            ifTrue: [
                text := 'After piecing together some of the fragments of the city''s history, you decide to leave. ' ,
                        'Pathetic, isn''t it? ' ,
                        'Either way, the city''s shadows grow darker with each discovery, hinting at forces far beyond human understanding. ' ,
                        'You realized the danger, deciding to abandon the case, leaving the mysteries of the City of Shadows unsolved. ' ,
                        'That might''ve been a wise decision—who knows? ' ,
                        'The chilling secrets of this forgotten town remain buried, but you manage to escape with your life, haunted by unanswered questions.'.
            ]
            ifFalse: [
                text := 'You haven''t learned anything yet decided to leave this town. ' ,
                        'Pathetic, isn''t it? ' ,
                        'Either way, the city''s shadows grow darker with each discovery, hinting at forces far beyond human understanding. ' ,
                        'You realized the danger, deciding to abandon the case, leaving the mysteries of the City of Shadows unsolved. ' ,
                        'That might''ve been a wise decision—who knows? ' ,
                        'The chilling secrets of this forgotten town remain buried, but you manage to escape with your life, haunted by unanswered questions.'.
            ].

        ^text
    ]

    Endings class >> hillChurchEndingEscapeText [
        ^'You''ve discovered everything that was possible to be discovered. ' ,
          'Yet, you decided to leave. Might''ve been wise, or maybe fear drove you to leave this city for its doom. ' ,
          'So be it, farewell.'
    ]

    Endings class >> forestCaveEndingWeakenedText [
        ^'The weird ring started to glow, giving you the power to destroy all of the cults places. ' ,
          'Armed with every piece of the puzzle, the detective confronts the heart of the sects power. ' ,
          'Having the power of the ring, you easily destroy the most powerful place of the cult. ' ,
          'The city, now freed from its dark binds, is left desolate and cursed, but you walk away victorious. ' ,
          'The shadows may linger, but the sects grip on the City of Shadows is broken, its haunting legacy fading into history.'
    ]

    Endings class >> forestCaveEndingKilledText [
        ^'The cult for long knew that you were on their tails. ' ,
          'Yet, as you finally grasp the full horror of the sect''s motives, you are caught in a trap, surrounded by the sects followers. ' ,
          'Expecting you, they ambush and easily kill you. ' ,
          'Your journey ends here, as they are drawn into the dark embrace of the city, their fate forever tied to its haunting secrets.'
    ]
]
