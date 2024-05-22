
## select leegstand voor Aanpak Noord en Masterplannen

library(tidyverse)
library(openxlsx)

tabel<- read_rds("04 output tabellen/tabel_locatus.rds")

my_filter<- function(x){
  
  x |>
    filter(
      sectoren == 'leegstand',
      name %in% c("aandeel vestigingen (%)", "aandeel oppervlakte (%)")
    )
}

tabel_leegstand <- tabel |>
  map(\(x) my_filter(x))


tabel_leegstand$wijk<- tabel_leegstand$wijk|>
  filter(
    gbd_wijk_code != 'Amsterdam',
    !is.na(gbd_wijk_code))|>
  rename(
    spatial_code= gbd_wijk_code,
    spatial_name= gbd_wijk_naam) |>
  pivot_longer(
    cols=c(`2014`:`2024`), 
    names_to = "temporal_date")


tabel_leegstand$gebied<- tabel_leegstand$gebied|>
  filter(
    gbd_ggw_code != 'Amsterdam',
    !is.na(gbd_ggw_code))|>
  rename(
    spatial_code= gbd_ggw_code,
    spatial_name= gbd_ggw_naam)|>
  pivot_longer(
    cols=c(`2014`:`2024`), 
    names_to = "temporal_date")

tabel_leegstand$stadsdeel<- tabel_leegstand$stadsdeel|>
  
  filter(
    !is.na(gbd_sdl_code))|>
  
  mutate(gbd_sdl_code= case_when(
    gbd_sdl_code == "Amsterdam" ~ '0363',
    TRUE                        ~ gbd_sdl_code))|>
  
  rename(
    spatial_code= gbd_sdl_code,
    spatial_name= gbd_sdl_naam)|>
  pivot_longer(
    cols=c(`2014`:`2024`), 
    names_to = "temporal_date")

df_leegstand<- bind_rows(
  
  tabel_leegstand$stadsdeel,
  tabel_leegstand$gebied,
  tabel_leegstand$wijk 
  )|>
  
  add_column(spatial_date = '20220324',
             tweedeling = 'totaal')|>
  
  
  mutate(
    
    temporal_date=glue::glue("{temporal_date}0101"),
    
    meausure=case_when(
      name=='aandeel vestigingen (%)' ~ 'BHLOCVKPLEEGSTAND_P',
      name=='aandeel oppervlakte (%)' ~ 'BHLOCOPPLEEGSTAND_P')) |>
  
  select(-(c("sectoren", "name")))
write.xlsx(df_leegstand, "04 output tabellen/data_leegstand.xlsx")

