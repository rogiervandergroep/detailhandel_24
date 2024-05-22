

### selectie locatus data voor dataverhaal


library(tidyverse)
library(openxlsx)

tabel<- read_rds("04 output tabellen/tabel_locatus.rds")

#verkooppunten en oppervlakte 
tabel_vest_opp <- tabel$stadsdeel |>
    filter(
      !is.na(gbd_sdl_code),
      sectoren %in% c(
       'detailhandel dagelijks', 
       'detailhandel niet-dagelijks'),
       name %in% c(
         'aantal vestigingen',
         'oppervlakte (m2)')) |>
  pivot_longer(
    cols= c(`2014`:`2024`), 
    names_to = 'jaar', values_to = 'waarde')

      
#verkooppunten en oppervlakte 
tabel_leegstand_sd <- tabel$stadsdeel |>
  filter(
    !is.na(gbd_sdl_code),
    sectoren %in% c(
      'leegstand'),
    name %in% c(
      'aantal vestigingen',
      'oppervlakte (m2)',
      'aandeel vestigingen (%)',
      'aandeel oppervlakte (m2) (%)')) |>
  pivot_longer(
    cols= c(`2014`:`2024`), 
    names_to = 'jaar', values_to = 'waarde') 

tabel_leegstand_wijk <- tabel$wijk |>
  filter(
    !is.na(gbd_wijk_code),
    sectoren %in% c(
      'leegstand'),
    name %in% c(
      'aantal vestigingen',
      'oppervlakte (m2)',
      'aandeel vestigingen (%)',
      'aandeel oppervlakte (m2) (%)')) |>
  pivot_longer(
    cols= c(`2014`:`2024`), 
    names_to = 'jaar', values_to = 'waarde') 
      

list_dataverhaal <- read_rds("04 output tabellen/tabel_dataverhaal.rds")

list_dataverhaal$vest_opp  <- tabel_vest_opp
list_dataverhaal$leegstand_sd <- tabel_leegstand_sd
list_dataverhaal$leegstand_wijk <- tabel_leegstand_wijk

write_rds(list_dataverhaal, "04 output tabellen/tabel_dataverhaal.rds")

  