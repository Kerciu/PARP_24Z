% Definicje lokacji: każda lokacja jest unikalna i posiada opis oraz połączenia do innych lokacji

% Lokalizacja: Stacja Kolejowa
location(stacja_kolejowa, 
    "Stacja kolejowa. Tu zaczyna się Twoja przygoda, ponure miejsce, pełne cienia i kurzu. Na peronie stoi dozorczyni.",
    [npc(dozorczyni)],
    [wschod -> glowna_ulica]
).

% Lokalizacja: Główna Ulica
location(glowna_ulica,
    "Główna ulica miasta, pełna opuszczonych i zakurzonych sklepów. Możesz usłyszeć echo kroków zagubionego wędrowca.",
    [npc(wedrowiec)],
    [zachod -> stacja_kolejowa, polnoc -> posterunek_policji, poludnie -> biblioteka]
).

% Lokalizacja: Posterunek Policji
location(posterunek_policji,
    "Opuszczony posterunek policji. W zakurzonych archiwach znajdziesz notatki dotyczące archeologów i miasta.",
    [],
    [poludnie -> glowna_ulica, wschod -> stary_hotel]
).

% Lokalizacja: Biblioteka Miejska
location(biblioteka,
    "Stara biblioteka pełna zakurzonych ksiąg. W podziemiach skrywa stare kroniki i tajemnicze historie miasta.",
    [],
    [polnoc -> glowna_ulica]
).

% Lokalizacja: Stary Hotel
location(stary_hotel,
    "Tajemniczy, opuszczony hotel na skraju miasta. W środku znajdują się dziwne symbole, a w jednym z pokojów notatki lidera archeologów.",
    [],
    [zachod -> posterunek_policji, polnoc -> kosciol_na_wzgorzu]
).

% Lokalizacja: Kościół na Wzgórzu
location(kosciol_na_wzgorzu,
    "Nieczynny kościół, pełen echa dawnych rytuałów sekty. Spotkasz tu proboszcza, który zna sekrety miasta.",
    [npc(proboszcz)],
    [poludnie -> stary_hotel, wschod -> jaskinia]
).

% Lokalizacja: Jaskinia w Lesie
location(jaskinia,
    "Mroczna jaskinia w lesie. Na środku znajduje się ołtarz z symbolem przypominającym starożytny artefakt poszukiwany przez archeologów.",
    [],
    [zachod -> kosciol_na_wzgorzu]
).