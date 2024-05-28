

### selectie locatus data voor dataverhaal


library(tidyverse)
library(openxlsx)

tabel<- read_rds("04 output tabellen/tabel_locatus.rds")

#verkooppunten en oppervlakte 
tabel_vest_opp <- tabel$stadsdeel |>
    filter(
      !is.na(gbd_sdl_code),
      name %in% c(
         'aantal vestigingen',
         'oppervlakte (m2)')) |>
  pivot_longer(
    cols= c(`2014`:`2024`), 
    names_to = 'jaar', values_to = 'waarde')|>
  rename(
    gebieds_code = gbd_sdl_code,
    gebieds_naam = gbd_sdl_naam)

      
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
      'aandeel oppervlakte (%)')) |>
  pivot_longer(
    cols= c(`2014`:`2024`), 
    names_to = 'jaar', values_to = 'waarde') |>
  mutate(gebieds_type=case_when(
    gbd_sdl_code == 'Amsterdam' ~ 'stad',
    TRUE ~ 'stadsdeel' ))|>
  rename(
    gebieds_code = gbd_sdl_code,
    gebieds_naam = gbd_sdl_naam)

tabel_leegstand_wijk <- tabel$wijk |>
  filter(
    gbd_wijk_code != 'Amsterdam', 
    !is.na(gbd_wijk_code),
    sectoren %in% c(
      'leegstand'),
    name %in% c(
      'aantal vestigingen',
      'oppervlakte (m2)',
      'aandeel vestigingen (%)',
      'aandeel oppervlakte (%)')) |>
  pivot_longer(
    cols= c(`2014`:`2024`), 
    names_to = 'jaar', values_to = 'waarde') |>
  rename(
    gebieds_code = gbd_wijk_code,
    gebieds_naam = gbd_wijk_naam)|>
  add_column(gebieds_type='wijk')

tabel_leegstand <- bind_rows(tabel_leegstand_sd, tabel_leegstand_wijk)

write.csv2(tabel_leegstand, "04 output tabellen/tabel_dataverhaal_leegstand.csv")

list_dataverhaal <- read_rds("04 output tabellen/tabel_dataverhaal.rds")

list_dataverhaal$vest_opp  <- tabel_vest_opp
list_dataverhaal$leegstand <- tabel_leegstand

write_rds(list_dataverhaal, "04 output tabellen/tabel_dataverhaal.rds")
write.xlsx(list_dataverhaal,"04 output tabellen/tabel_dataverhaal.xlsx", withFilter=T, overwrite = T) 
  