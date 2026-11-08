library(dplyr)
# ZADANIE 7
#Wybór dwóch kategorii miejscowości
cbos_7 <- cbos %>%
  filter(miejscowosc %in% c("wieś", "miasto 500 000 i więcej"))

# Liczba respondentów w każdej grupie
liczebnosci_7 <- cbos_7 %>%
  count(miejscowosc, name = "liczba_respondentow")

liczebnosci_7

# Rozkład odpowiedzi i procenty wewnątrz każdej grupy
rozkład_7 <- cbos_7 %>%
  count(miejscowosc, kierunek, name = "liczba") %>%
  group_by(miejscowosc) %>%
  mutate(
    liczebnosc_grupy = sum(liczba),
    procent = 100 * liczba / liczebnosc_grupy
  ) %>%
  ungroup()

rozkład_7 %>%
  mutate(procent = round(procent, 2))

# Porównanie odpowiedzi "W złym"
porownanie_7 <- rozkład_7 %>%
  filter(kierunek == "W złym")

porownanie_7 %>%
  mutate(procent = round(procent, 2))

# Automatyczny wniosek
p_wies <- porownanie_7 %>%
  filter(miejscowosc == "wieś") %>%
  pull(procent)

p_miasto <- porownanie_7 %>%
  filter(miejscowosc == "miasto 500 000 i więcej") %>%
  pull(procent)

if (length(p_wies) == 1 && length(p_miasto) == 1) {
  cat(sprintf(
    paste0(
      "\nOdpowiedź „W złym” wskazało %.2f%% respondentów ze wsi ",
      "oraz %.2f%% respondentów z miast liczących co najmniej ",
      "500 tys. mieszkańców.\n",
      "Różnica (duże miasta minus wieś) wynosi %.2f punktu procentowego.\n"
    ),
    p_wies, p_miasto, p_miasto - p_wies
  ))
  
  cat(
    if (isTRUE(all.equal(p_wies, p_miasto))) {
      "Udziały były takie same.\n"
    } else {
      "Udziały nie były takie same.\n"
    }
  )
}

# ZADANIE 8
# Rozkład prognoz w całej próbie
prognoza_wszyscy <- cbos %>%
  count(prognoza, sort = TRUE, name = "liczba")

prognoza_wszyscy

# Osoby oceniające obecny kierunek jako "W złym"
cbos_zly <- cbos %>%
  filter(kierunek == "W złym")

# Liczebność tej grupy
nrow(cbos_zly)

# Rozkład prognoz w tej grupie
prognoza_zly <- cbos_zly %>%
  count(prognoza, sort = TRUE, name = "liczba") %>%
  mutate(
    procent = 100 * liczba / sum(liczba)
  )

prognoza_zly %>%
  mutate(procent = round(procent, 2))

# Najczęstsza niebrakująca odpowiedź; uwzględnia ewentualne remisy
najczestsza_8 <- prognoza_zly %>%
  filter(!is.na(prognoza)) %>%
  slice_max(order_by = liczba, n = 1, with_ties = TRUE)

najczestsza_8

# Automatyczny wniosek
cat(sprintf(
  "\nLiczba osób oceniających kierunek zmian jako „W złym”: %d.\n",
  nrow(cbos_zly)
))

for (i in seq_len(nrow(najczestsza_8))) {
  cat(sprintf(
    "Najczęstsza odpowiedź: „%s” — %d osób (%.2f%% tej grupy).\n",
    as.character(najczestsza_8$prognoza[i]),
    najczestsza_8$liczba[i],
    najczestsza_8$procent[i]
  ))
}