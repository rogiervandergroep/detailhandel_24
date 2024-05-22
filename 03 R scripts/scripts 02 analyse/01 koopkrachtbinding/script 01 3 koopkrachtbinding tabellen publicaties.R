
### samenvattingen tbv figuren  ---
### binding met eigen stadsdeel ---

library(tidyverse)
library(openxlsx)

tabel_sd    <- read_rds ("04 output tabellen/tabel_kkb_sd_20tm24.rds")




tabel_samengevat <- bind_rows(
  
  # eigen sd of online
  bind_rows(tabel_sd)|>
    filter (gbd_sdl_naam == afzet_stadsdeel_naam | 
              afzet_stadsdeel_naam == 'online'),
  
  # afvloeiing overig amsterdam
  bind_rows(tabel_sd)|>
    filter (gbd_sdl_naam != afzet_stadsdeel_naam,
            afzet_stadsdeel_naam %in% 
              c('Centrum', 'West', 'Nieuw-West', "Zuid",
                'Noord', 'Oost', 'Zuidoost', 'Weesp'))|>
    group_by(monitor, periode, gbd_sdl_naam )|>
    summarise(across(`dagelijkse producten`: `sport en spel`, sum)) |>
    add_column(afzet_stadsdeel_naam= 'Amsterdam'),
  
  
  ### afvloeiing buiten Ams
  bind_rows(tabel_sd)|>
    filter (afzet_stadsdeel_naam %in% 
              c('MRA', 'overig NL'))|>
    group_by(monitor, periode, gbd_sdl_naam, afzet_stadsdeel_naam )|>
    summarise(across(`dagelijkse producten`: `sport en spel`, sum))
)|>
  select(-afzet_stadsdeel_code) |>
  mutate(across(where(is.numeric), ~ round(.)))



### binding Centrum, overig Amsterdam etc door totaal amsterdammers
tabel_centrum <- bind_rows(
  
  # orientatie Centrum
  tabel_sd$`totaal Amsterdam`|>
    filter (afzet_stadsdeel_naam == 'Centrum' | 
              afzet_stadsdeel_naam == 'online'),
  
  # afvloeiing overig amsterdam
  tabel_sd$`totaal Amsterdam`|>
    filter (afzet_stadsdeel_naam %in% c('West','Nieuw-West',"Zuid",
                                        'Noord','Oost','Zuidoost','Weesp'))|>
    group_by(monitor, periode, gbd_sdl_naam )|>
    summarise(across(`dagelijkse producten`: `sport en spel`, sum)) |>
    add_column(afzet_stadsdeel_naam= 'Amsterdam'),
  
  
  ### afvloeiing buiten Ams
  tabel_sd$`totaal Amsterdam`|>
    filter (afzet_stadsdeel_naam %in% 
              c('MRA', 'overig NL'))|>
    group_by(monitor, periode, gbd_sdl_naam, afzet_stadsdeel_naam )|>
    summarise(across(`dagelijkse producten`: `sport en spel`, sum))
)|>
  select(-afzet_stadsdeel_code)|>
  mutate(across(where(is.numeric), ~ round(.)))


#################################
### factor-levels tbv figuren ---
#################################

name_lev <- c("dagelijkse producten","modeartikelen","elektronica",          
              "huishoudelijke artikelen","doe het zelf","woninginrichting",
              "planten en bloemen","media en vrijetijd",
              "sport en spel",
              "niet-dagelijkse producten recreatief", "niet-dagelijkse producten doelgericht", "niet-dagelijkse producten totaal") 

#herkomst -
gbd_sdl_naam_lev <- c("Centrum", "West","Nieuw-West","Zuid","Oost",
                      "Noord","Zuidoost","Weesp","totaal Amsterdam")

# afzet -
afzet_sdl_naam_lev <- c("Centrum","West","Nieuw-West","Zuid","Oost",
                        "Noord","Zuidoost","Weesp",
                        "Amsterdam","MRA","overig NL","online")

# afzet eigen sd -
afzet_eigen_sd_lev <- c("eigen stadsdeel", "Amsterdam","MRA","overig NL","online")


tabel_samengevat_l <- tabel_samengevat |>
  pivot_longer(cols = c(`dagelijkse producten`: `sport en spel`)) |>
  
  mutate(
    name=case_when(
      name == 'NDG recreatief'  ~ 'niet-dagelijkse producten recreatief',
      name == 'NDG doelgericht' ~ 'niet-dagelijkse producten doelgericht',
      name == 'NDG totaal'      ~ 'niet-dagelijkse producten totaal', 
      TRUE ~ name),
    
    gbd_sdl_naam= factor(
      gbd_sdl_naam, levels = gbd_sdl_naam_lev),
    
    afzet_eigen_sd = case_when(
      gbd_sdl_naam == afzet_stadsdeel_naam ~ 'eigen stadsdeel',
      TRUE ~ afzet_stadsdeel_naam),
    
    afzet_stadsdeel_naam= factor(
      afzet_stadsdeel_naam, levels = afzet_sdl_naam_lev),
    
    name = factor(
      name, levels = name_lev),
    
    afzet_eigen_sd = factor(
      afzet_eigen_sd, levels = afzet_eigen_sd_lev))


tabel_centrum_l <- tabel_centrum |>
  pivot_longer(cols= c(`dagelijkse producten`:`sport en spel`)) |>
  
  mutate(
    name=case_when(
      name == 'NDG recreatief'  ~ 'niet-dagelijkse producten recreatief',
      name == 'NDG doelgericht' ~ 'niet-dagelijkse producten doelgericht',
      name == 'NDG totaal'      ~ 'niet-dagelijkse producten totaal',  
      TRUE ~ name),
    
    afzet_stadsdeel_naam= factor(
      afzet_stadsdeel_naam, levels = afzet_sdl_naam_lev),
    
    name = factor(
      name, levels = name_lev))


list_dataverhaal <- list()


list_dataverhaal$kkb_sd_20_24 <- tabel_samengevat_l



### voor tabel publicatie en dataverhaal ---




tabel_samengevat_l |>
  pivot_wider (values_from = value, names_from = name)|>
  write.xlsx("04 output tabellen/tabel_kkb_sd_20tm24_samenvatting2.xlsx", overwrite = T)
tabel_samengevat |>
  write.xlsx("04 output tabellen/tabel_kkb_sd_20tm24_samenvatting.xlsx", withFilter=T, overwrite = T)
tabel_centrum |>
  write.xlsx("04 output tabellen/tabel_kkb_ams_centrum.xlsx", withFilter=T, overwrite = T)

save(tabel_samengevat_l, tabel_centrum_l, file = "03 tussentijds/tabel_kkb_samenvatting.Rdata")

# overzicht 2012 - 2024 

tabel_12_24 <- read.xlsx("03 tussentijds/kkb_ams12_24.xlsx")|>
  group_by(sdl_centrum,	productgroep,	jaar)|>
  summarise(waarde= sum(waarde))


list_dataverhaal$kkb_ams_12_24 <- tabel_12_24



saveRDS(list_dataverhaal, "04 output tabellen/tabel_dataverhaal.rds")

