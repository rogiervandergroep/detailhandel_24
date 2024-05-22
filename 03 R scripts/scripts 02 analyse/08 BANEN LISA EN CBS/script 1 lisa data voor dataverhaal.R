
library(tidyverse)
library(openxlsx)

## lisa data voor dataverhaal ---

tabel_totaal<- readxl::read_xlsx("04 output tabellen/tabel_lisa13_23.xlsx")|>
  group_by(peildatum, sdl_code, sdl_naam, sector) |>
  summarise(
    waarde=sum(aantal_banen))



list_dataverhaal <- read_rds("04 output tabellen/tabel_dataverhaal.rds")

list_dataverhaal$banen  <- tabel


write_rds(list_dataverhaal, "04 output tabellen/tabel_dataverhaal.rds")
write.xlsx(list_dataverhaal,"04 output tabellen/tabel_dataverhaal.xlsx", withFilter=T, overwrite = T) 
