```{r instalacja, eval=FALSE}
# Wykonaj tylko raz, jeżeli brakuje pakietów.
install.packages(c("dplyr", "tidyr", "haven"))
```
library(tidyverse)

## 3. CBOS: jak respondenci oceniali sytuację w Polsce w 2017 r.?

Źródło: [plik CBOS w formacie SPSS](https://github.com/kflisikowsky/sad/blob/main/data/CBOS_201733_331_SPSS.sav) i [dokumentacja badania](https://github.com/kflisikowsky/sad/blob/main/data/CBOS_201733_331_DOK.pdf) w repozytorium prowadzącego. W pliku znajduje się **925 respondentów i 189 zmiennych**. Do dzisiejszych ćwiczeń wykorzystamy tylko kilka pytań.

| Kolumna źródłowa | Znaczenie |
  |:--|:--|
  | `plec` | płeć respondenta |
  | `klm6` | wielkość miejscowości |
  | `q24` | czy sytuacja w Polsce zmierza w dobrym czy złym kierunku |
  | `q25` | przewidywanie sytuacji w Polsce w kolejnym roku |
  | `q15` | poziom wykształcenia |
  
  SPSS przechowuje opisy kategorii odpowiedzi. Funkcja `haven::as_factor()` pozwala zobaczyć je zamiast kodów liczbowych. Odpowiedzi „Trudno powiedzieć” i „Odmowa odpowiedzi” są odrębnymi kategoriami — nie należy automatycznie zamieniać ich na `NA`.

```{r cbos-wczytanie}
library(haven)
adres_cbos <- paste0("https://raw.githubusercontent.com/kflisikowsky/sad/",
                     "main/data/CBOS_201733_331_SPSS.sav")
plik_cbos <- tempfile(fileext = ".sav")
download.file(adres_cbos, plik_cbos, mode = "wb", quiet = TRUE)
cbos_raw <- read_sav(plik_cbos)
unlink(plik_cbos)

cbos <- cbos_raw %>% transmute(
  plec = as_factor(plec),
  miejscowosc = as_factor(klm6),
  kierunek = as_factor(q24),
  prognoza = as_factor(q25),
  wyksztalcenie = as_factor(q15)
)
dim(cbos)
head(cbos)
tail(cbos)
```

### Zadanie 5 — poznaj respondentów

Sprawdź liczbę obserwacji i braków. Przygotuj tabelę liczności i odsetków dla `plec` oraz `miejscowosc`. 
Jaki odsetek ankietowanych mieszkał na wsi, a jaki w miastach liczących co najmniej 500 tys. mieszkańców? 
Licz odsetki względem liczby **niebrakujących odpowiedzi** w danej zmiennej i podaj użyty mianownik.

```{r cbos-zad-5}
# WASZE ROZWIĄZANIE
# count(), mutate(procent = 100 * n / sum(n)), filter(!is.na(...))
nrow(cbos)
colSums(is.na(cbos))
table(cbos$plec)
tab_plec <- data.frame(
  Kategoria = names(table(cbos$plec)),
  Liczebnosc = as.vector(table(cbos$plec)),
  Odsetek = round(
    as.vector(prop.table(table(cbos$plec[!is.na(cbos$plec)])) * 100),
    2
  )
)

print(tab_plec)

tab_miejscowosc <- data.frame(
  Kategoria = names(table(cbos$miejscowosc)),
  Liczebnosc =as.vector(table(cbos$miejscowosc)),
  Odsetek = round(
    as.vector(prop.table(table(cbos$miejscowosc[!is.na(cbos$miejscowosc)])) * 100),
    2
  )
)

print(tab_miejscowosc)

wies  <-nrow(cbos[cbos$miejscowosc == "wieś", ])/925*100
miasto_500k <- nrow(cbos[cbos$miejscowosc == "miasto 500 000 i więcej", ])/925*100
#użyty mianownik - 925

```

### Zadanie 6 — jak oceniano kierunek zmian w kraju?

Oblicz liczebności i odsetki odpowiedzi na pytanie `kierunek`. 
Pokaż osobno odpowiedzi „W dobrym”, „W złym”, „Trudno powiedzieć” i odmowy, jeśli występują. Która odpowiedź była najczęstsza?
Następnie policz udział odpowiedzi „W dobrym” **tylko wśród osób, które wybrały „W dobrym” albo „W złym”**. 
Porównaj go z odsetkiem w całej próbie. Wyjaśnij różnicę w mianowniku.

```{r cbos-zad-6}
# WASZE ROZWIĄZANIE
# count(kierunek), filter(kierunek %in% c("W dobrym", "W złym"))

tab_kierunek <- data.frame(
  Kategoria = names(table(cbos$kierunek)),
  Liczebnosc =as.vector(table(cbos$kierunek)),
  Odsetek = round(
    as.vector(prop.table(table(cbos$kierunek[!is.na(cbos$kierunek)])) * 100),
    2
  )
)

print(tab_kierunek)

table(cbos$kierunek)
#najczęstrza odpowiedz - "w dobrym" (432 odp.)

wybrane <- cbos %>%
  filter(kierunek %in% c("W dobrym", "W złym")) %>%
  count(kierunek)

odsetek_wybrane <- wybrane %>%
  filter(kierunek == "W dobrym") %>%
  pull(n) / sum(wybrane$n) * 100

calosc <- cbos %>%
  count(kierunek)

odsetek_calosc <- calosc %>%
  filter(kierunek == "W dobrym") %>%
  pull(n) / nrow(cbos) * 100

#różnica w mianowniku - "odsetek wybrane" mianownik (777): suma odp. "w dobrym", "w złym" ; "odsetek calosc" mianownik (925): suma wszystkich odp. (nie było brakujących odp.) 

```
