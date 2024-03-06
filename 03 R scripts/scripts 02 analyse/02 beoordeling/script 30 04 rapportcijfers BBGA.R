
library(openxlsx)
library(tidyverse)

# data inladen en wijken aan gebieden koppelen ----


# inlezen namen rapportcijfers -
tab_rap_naam <- read.xlsx(
  "02 lookup tabellen/format rapportcijfers namen.xlsx") |>
  mutate(name= str_trim(name, side = "both")) |>
  distinct(name  , .keep_all = T) |>
  mutate(item= str_trim(name_new, side = "both"))

# inlezen naamgeving BBGA
tab_rnaam_measure <- read.xlsx(
  "02 lookup tabellen/format rapportcijfers namen.xlsx", sheet = "BBGA") |>
  mutate(item= str_trim(name_new, side = "both")) |>
  mutate(measure= str_trim(measure, side = "both"))


 

# rapportcijfers 2024 -
rapportcijfers_24_raw   <- read.xlsx ("04 output tabellen/tabel_rapportcijfers24_lng.xlsx")


# rapportcijfers 2024 -
rapportcijfers_24   <- read.xlsx (
  "04 output tabellen/tabel_rapportcijfers24_lng.xlsx") |>
  
  add_column(periode= "monitor 2022 2023")|>
  mutate(spatial_code = winkelgebied_oiscode, 
         spatial_name= winkelgebied_oisnaam,
         value = gemiddelde) |>
  left_join(tab_rnaam_measure, by= "item")|>
  add_column(
    spatial_type  = 'winkelgebieden',
    spatial_date  = '20240101',
    temporal_date = '20240101',
    temporal_type = 'peildatum')|>
  
    select(spatial_code,	spatial_name,	spatial_type,	spatial_date,	temporal_date,	temporal_type, 	measure, value)|>
  filter(!is.na(spatial_code))


write.csv2(rapportcijfers_24, "04 output tabellen/20240101_rapwingeb_cijfers.csv")
