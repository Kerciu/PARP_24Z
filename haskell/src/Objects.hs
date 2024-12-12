module Objects where
import Interactable

cigarettes :: Interactable
cigarettes = Interactable {
    name = "Cigarettes",
    description = "The pack of cigarettes is old, with some of the edges frayed. It might be useful to someone who needs a smoke."
}

weirdBox :: Interactable
weirdBox = Interactable {
    name = "Weird Box",
    description = "A box with a lock that opens with more than 20 digits." ++
        "Impossible to open for now, but maybe it will prove useful in the future."
}

harnas :: Interactable
harnas = Interactable {
    name = "Harnas Beer",
    description = "A can of Harnas beer, cold and seemingly untouched. It might help you in your interactions with certain characters."
}

kufloweMocne :: Interactable
kufloweMocne = Interactable {
    name = "Kuflowe Mocne",
    description = "A bottle of Kuflowe Mocne, a strong beer. It might be useful for someone who needs a drink."
}

carKeys :: Interactable
carKeys = Interactable {
    name = "Car Keys",
    description = "A set of car keys. They seem to belong to the abandoned car in the parking lot. Maybe you can unlock it."
}

amulet :: Interactable
amulet = Interactable {
    name = "Amulet",
    description = "A strange amulet, cold to the touch. It seems to have an otherworldly aura, as if it carries a hidden power."
}

diary :: Interactable
diary = Interactable {
    name = "Diary",
    description = "The diary is yellowed and fragile." ++
    "Some pages are barely readable, but one section stands out, scribbled with urgency:" ++
    "" ++
    "\"We have found traces of an entrance near the old oak in the forest." ++
    "It must be the cave mentioned in the legends... The symbols match." ++
    "It is said the cave holds more than relics — perhaps a power that should remain undisturbed." ++
    "We must proceed with caution.\""
}

notes :: Interactable
notes = Interactable {
    name = "Notes",
    description = "The notes are brittle and covered in dust, with several pages missing or torn." ++
    "" ++
    "\"We found it — the amulet from the old texts." ++
    "It is said this amulet is the key to something ancient, hidden deep within the caves." ++
    "We must be cautious. The energy here is unstable, and there are warnings in the old language about disturbing what lies beyond.\""
}

newspaper :: Interactable
newspaper = Interactable {
    name = "Newspaper",
    description = "You start reading it. The headline catches your eye:" ++
    "\"Team of Archaeologists Arrives to Uncover the Secrets of the Ancient Ruins in Town.\"" ++
    "" ++
    "The article mentions several archaeologists by name:" ++
    "- Sarah Miller (born 1952)" ++
    "- John Roberts (born 1968)" ++
    "- Michael Turner (born 1974)" ++
    "- Emma Carter (born 1980)"
}

leafWithCode :: Interactable
leafWithCode = Interactable {
    name = "Leaft with Code",
    description = "The leaf contains a mysterious riddle:" ++
    "The code is hidden in two pairs of numbers." ++
    "The first pair comes from multiplying three by the digit symbolizing fullness." ++
    "The second pair is a sequence you receive from the natural order." ++
    "You think about these pairs to solve the code."
}

engravedRing :: Interactable
engravedRing = Interactable {
    name = "Engraved Ring",
    description = "The ring is engraved with strange symbols and feels unusually heavy in your hand." ++
    "It might hold some hidden significance or power."
}

key :: Interactable
key = Interactable {
    name = "Key",
    description = "A small, rusted key. It might be useful for unlocking something."
}
