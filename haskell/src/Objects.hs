module Objects where

import Data.Map qualified as Map
import Interactable

findItem :: String -> Maybe Interactable
findItem itemName = Map.lookup itemName itemsMap
  where
    itemsMap =
      Map.fromList
        [ ("cigarettes", cigarettes),
          ("weird_box", weirdBox),
          ("harnas", harnas),
          ("kuflowe_mocne", kufloweMocne),
          ("car_keys", carKeys),
          ("amulet", amulet),
          ("diary", diary),
          ("notes", notes),
          ("newspaper", newspaper),
          ("leaf_with_code", leafWithCode),
          ("engraved_ring", engravedRing),
          ("key", key),
          ("red_fuse", redFuse),
          ("green_fuse", greenFuse),
          ("blue_fuse", blueFuse)
        ]

cigarettes :: Interactable
cigarettes =
  Interactable
    { name = "cigarettes",
      description = "The pack of cigarettes is old, with some of the edges frayed. It might be useful to someone who needs a smoke."
    }

weirdBox :: Interactable
weirdBox =
  Interactable
    { name = "weird_box",
      description = unlines [
        "A box with a lock that opens with more than 20 digits.",
        "Impossible to open for now, but maybe it will prove useful in the future."]
    }

harnas :: Interactable
harnas =
  Interactable
    { name = "harnas_beer",
      description =  unlines [
      "A can of Harnas beer, cold and seemingly untouched",
      "It might help you in your interactions with certain characters."]
    }

kufloweMocne :: Interactable
kufloweMocne =
  Interactable
    { name = "kuflowe_mocne",
      description = unlines [
      "A bottle of Kuflowe Mocne, a strong beer.",
      "It might be useful for someone who needs a drink."]
    }

carKeys :: Interactable
carKeys =
  Interactable
    { name = "car_keys",
      description = "A set of car keys. They seem to belong to the abandoned car in the parking lot. Maybe you can unlock it."
    }

amulet :: Interactable
amulet =
  Interactable
    { name = "amulet",
      description = "A strange amulet, cold to the touch. It seems to have an otherworldly aura, as if it carries a hidden power."
    }

diary :: Interactable
diary =
  Interactable
    { name = "diary",
      description = unlines [
        "The diary is yellowed and fragile.",
        "Some pages are barely readable, but one section stands out, scribbled with urgency:",
        "",
        "\"We have found traces of an entrance near the old oak in the forest.",
        "It must be the cave mentioned in the legends... The symbols match.",
        "It is said the cave holds more than relics — perhaps a power that should remain undisturbed.",
        "We must proceed with caution.\""]
    }

notes :: Interactable
notes =
  Interactable
    { name = "notes",
      description = unlines [
        "The notes are brittle and covered in dust, with several pages missing or torn.",
        "",
        "\"We found it — the amulet from the old texts.",
        "It is said this amulet is the key to something ancient, hidden deep within the caves.",
        "We must be cautious. The energy here is unstable, and there are warnings in the old language about disturbing what lies beyond.\""]
    }

newspaper :: Interactable
newspaper =
  Interactable
    { name = "newspaper",
      description = unlines [
        "You start reading it. The headline catches your eye:",
        "\"Team of Archaeologists Arrives to Uncover the Secrets of the Ancient Ruins in Town.\"",
        "",
        "The article mentions several archaeologists by name:",
        "- Sarah Miller (born 1952)",
        "- John Roberts (born 1968)",
        "- Michael Turner (born 1974)",
        "- Emma Carter (born 1980)"]
    }

leafWithCode :: Interactable
leafWithCode =
  Interactable
    { name = "leaf_with_code",
      description = unlines [
        "The leaf contains a mysterious riddle:",
        "The code is hidden in two pairs of numbers.",
        "The first pair comes from multiplying three by the digit symbolizing fullness.",
        "The second pair is a sequence you receive from the natural order.",
        "You think about these pairs to solve the code."]
    }

engravedRing :: Interactable
engravedRing =
  Interactable
    { name = "engraved_ring",
      description = unlines [
        "The ring is engraved with strange symbols and feels unusually heavy in your hand.",
        "It might hold some hidden significance or power."]
    }

key :: Interactable
key =
  Interactable
    { name = "key",
      description = "A small, rusted key. It might be useful for unlocking something."
    }

redFuse :: Interactable
redFuse =
  Interactable
    { name = "red_fuse",
      description = "A red fuse, used for electrical circuits. It might be useful for repairing something."
    }

greenFuse :: Interactable
greenFuse =
  Interactable
    { name = "green_fuse",
      description = "A green fuse, used for electrical circuits. It might be useful for repairing something."
    }

blueFuse :: Interactable
blueFuse =
  Interactable
    { name = "blue_fuse",
      description = "A blue fuse, used for electrical circuits. It might be useful for repairing something."
    }
