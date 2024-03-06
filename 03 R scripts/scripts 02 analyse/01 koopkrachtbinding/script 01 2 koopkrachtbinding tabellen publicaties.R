

library(tidyverse)
library(openxlsx)

freq_kkb_24 <- read_rds("03 tussentijds/freq_kkb_24.rds")

# toevoegen gebiedscode aan afzet_gebied en afzet_sd
gebiedsindeling_buurt <- read.csv2("02 lookup tabellen/gebiedsindeling2022.csv")

gebiedsindeling_sd <- gebiedsindeling_buurt |>
  select (gbd_sdl_code:gbd_sdl_naam) |>
  distinct(gbd_sdl_code, .keep_all = T) |>
  set_names("afzet_stadsdeel_code", "afzet_stadsdeel_naam") 

gebiedsindeling_gebied <- gebiedsindeling_buurt |>
  select (gbd_ggw_code:gbd_ggw_naam) |>
  distinct(gbd_ggw_code, .keep_all = T)|>
  set_names("afzet_gebied_code", "afzet_gebied_naam") 

# omzetten naar wide format toevoegen labels
wide_table <- list()


#### 
wide_table$ams <- freq_kkb_24$AMS_sd |>
  select(pdg_naam, afzet_stadsdeel_code, aandeel_gw_omz_ink) |>
  pivot_wider(names_from = pdg_naam, 
              values_from = aandeel_gw_omz_ink, 
              values_fill = 0) |>
  left_join(gebiedsindeling_sd, by= "afzet_stadsdeel_code") |>
  mutate(afzet_stadsdeel_naam=case_when(
    afzet_stadsdeel_code=='MRA'      ~ 'MRA',
    afzet_stadsdeel_code=='overig NL'~ 'overig NL',
    afzet_stadsdeel_code=='online'   ~ 'online',
    TRUE ~ afzet_stadsdeel_naam)
  )
    
wide_table$sd  <- freq_kkb_24$SD_sd |>
  select(pdg_naam, gbd_sdl_code, gbd_sdl_naam, afzet_stadsdeel_code, aandeel_gw_omz_ink) |>
  pivot_wider(names_from = pdg_naam, 
              values_from = aandeel_gw_omz_ink,
              values_fill = 0) |>
  left_join(gebiedsindeling_sd, by= "afzet_stadsdeel_code") |>
  mutate(afzet_stadsdeel_naam=case_when(
    afzet_stadsdeel_code=='MRA'      ~ 'MRA',
    afzet_stadsdeel_code=='overig NL'~ 'overig NL',
    afzet_stadsdeel_code=='online'   ~ 'online',
    TRUE ~ afzet_stadsdeel_naam)
  )

tabel_sd_24 <- wide_table$sd |>
  group_by(gbd_sdl_code, gbd_sdl_naam) |>
  group_split() |>
  set_names(unique(wide_table[["sd"]][["gbd_sdl_naam"]]))


# naar gebieden
wide_table$geb <- freq_kkb_24$GEB_geb|>
  select(pdg_naam, gbd_ggw_code, gbd_ggw_naam, afzet_gebied_code, aandeel_gw_omz_ink) |>
  pivot_wider(names_from = pdg_naam, 
              values_from = aandeel_gw_omz_ink,
              values_fill = 0)|>
  
  left_join(gebiedsindeling_gebied, by = "afzet_gebied_code") |>
  
  mutate(afzet_gebied_naam=case_when(
    afzet_gebied_code=='MRA'    ~ 'MRA',
    afzet_gebied_code=='overig NL'     ~ 'overig Nederland',
    afzet_gebied_code=='online' ~ 'online',
    TRUE ~ afzet_gebied_naam)
  )

tabel_geb_24 <- wide_table$geb |>
  group_by(gbd_ggw_code, gbd_ggw_naam) |>
  group_split() |>
  set_names(unique(wide_table[["geb"]][["gbd_ggw_code"]]))

write_rds(tabel_geb_24, "04 output tabellen/tabel_kkb_geb_24.rds")




# inlezen voorgaande jaren - 
file_path_sd <- ("02 lookup tabellen/tabel kkb sd 20 22.xlsx")  # dit is naar stadsdeel
file_path_gb <- ("02 lookup tabellen/tabel kkb gb 20 22.xlsx")  # dit is naar gebieden


# Get the names of all sheets in the Excel file
all_sheets_sd <- getSheetNames(file_path_sd)
all_sheets_gb <- getSheetNames(file_path_gb)

# maak een tabel voor de stadsdelen
tabel_sd_22 <- map(all_sheets_sd, ~ read.xlsx(file_path_sd, sheet = .x)) |>
  set_names(all_sheets_sd)

# maak een tabel voor de gebieden
tabel_gb_22 <- map(all_sheets_gb, ~ read.xlsx(file_path_gb, sheet = .x)) |>
  set_names(all_sheets_gb)

# voeg nieuwe blanco tabel toe voor weesp (obv format Centrum)
tabel_sd_22$Weesp <- data.frame(matrix(ncol = 14, nrow = 20)) |>
  set_names(names(tabel_sd_22$Centrum))






my_col_rename24 <- function (x){
  
  x |>
    select(-gbd_sdl_code)|>
    add_column(periode ="monitor 2022 2023")
}

tabel_sd_24 <- map(tabel_sd_24, my_col_rename24) 

# toevoegen amsterdam totaal 
tabel_sd_24$`totaal Amsterdam` <- wide_table$ams |>
  add_column(periode ="monitor 2022 2023",
             gbd_sdl_naam= 'totaal Amsterdam')
  




my_col_rename22 <- function (x){
  
  x |>
    rename(afzet_stadsdeel_code = sd15,
           afzet_stadsdeel_naam = sd15_naam,
           gbd_sdl_naam = stadsdeel_her,
           `dagelijkse producten`=v1,
           modeartikelen=w81_mode,
           elektronica=w82_elektro,
           `huishoudelijke artikelen`=w83_huish,
           woninginrichting=w84_woning,
           `doe het zelf`=w85_dhz,
           `planten en bloemen`=w86_planten,
           `media en vrijetijd`=w87_media,
           `sport en spel`=w88_sportspel,
           `NDG totaal` = NDG_totaal)
}

tabel_sd_22 <- map(tabel_sd_22, my_col_rename22)

sheet_order <- c("totaal Amsterdam", "Centrum", "West", "Nieuw-West", "Zuid", "Oost", "Noord", "Zuidoost", "Weesp")                 

tabel_sd_22 <- tabel_sd_22[sheet_order]
tabel_sd_24 <- tabel_sd_24[sheet_order]

tabel_sd <- map2(tabel_sd_22, tabel_sd_24, bind_rows)

my_monitor <- function(x){
  
  x |>
    
    mutate(
      monitor = case_when(
        periode == 'monitor 2019 2020' ~ 'monitor 2020',
        periode == 'monitor 2020 2021' ~ 'monitor 2022',
        periode == 'monitor 2022 2023' ~ 'monitor 2024'),
      periode= str_replace(periode, "monitor ", "veldwerkperiode ")
    ) |>
    select ("gbd_sdl_naam","monitor", "periode","afzet_stadsdeel_code", "afzet_stadsdeel_naam",
            "dagelijkse producten","NDG recreatief", "NDG doelgericht", "NDG totaal",
            "modeartikelen","elektronica","huishoudelijke artikelen", "woninginrichting","doe het zelf", 
            "planten en bloemen","media en vrijetijd","sport en spel")
}



### tabel binding eigen stadsdeel   
tabel_sd <- map(tabel_sd, my_monitor)  
write.xlsx(tabel_sd, "04 output tabellen/tabel_kkb_sd_20tm24.xlsx", withFilter=T, overwrite = T)
write_rds (tabel_sd, "04 output tabellen/tabel_kkb_sd_20tm24.rds")
