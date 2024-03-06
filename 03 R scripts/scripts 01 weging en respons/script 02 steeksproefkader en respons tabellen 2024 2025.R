
#lees libraries in met setup
#source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

library(tidyverse)


### aantal inwoners nieuwe wijkindeling 2022 en inwoners 2023

bev24 <- read.csv2("02 lookup tabellen/bevolking24tall.csv")
  
bev24_wijk <- bev24 |> 
  filter(
    measure =='Bevtotaal', 
    spatial_type == 'Wijk')|>
  select(spatial_code, value)|>
  mutate(
    spatial_code= replace_na(spatial_code, "NA"),
    value=as.integer(value))



# inlezen pc6 en wijkdata
pc6wijk22 <- read.csv2("02 lookup tabellen/postcode6 naar alle indelingen 2022 alternatief aangepast.csv")  |>
  mutate(gbd_wijk_code= replace_na(gbd_wijk_code, "NA"))

# lijst met alle nieuwe wijken en gebieden
wijk22<- pc6wijk22 |>
  group_by(gbd_wijk_code, gbd_wijk_naam, gbd_ggw_code, gbd_ggw_naam, gbd_sdl_code, gbd_sdl_naam)|>
  summarise(aantal=n())|>
  select(gbd_sdl_code, gbd_sdl_naam, gbd_ggw_code, gbd_ggw_naam, gbd_wijk_code, gbd_wijk_naam) 

# koppel gebiedsindeling aan inwoner per wijkdata
steekproefkader <- wijk22 |>
  left_join(
    bev24_wijk, 
    by=c("gbd_wijk_code" = "spatial_code")) |>
  rename(inwoners_24= value)|>
  
 #wegfilteren 
  filter(gbd_sdl_code  != "B", 
         gbd_wijk_code != 'FA', 
         gbd_wijk_code != 'NP',
         gbd_wijk_code != 'MK',
         gbd_wijk_code != 'EA',
         gbd_wijk_code != 'TA',
         gbd_wijk_code != 'FF') |>
  
 

  group_by(gbd_ggw_code,  gbd_ggw_naam)|>
  mutate(inw_geb24 = sum(inwoners_24))|>
  mutate(target_totaal=inwoners_24/inw_geb24*200) |>
  
  # omdat er vier veldwerktranches zijn (waarvan een via panel)
  
  mutate(target_t1 = round(target_totaal/4))

# check totale target en target 1
steekproefkader|>
  ungroup()|>
  summarise(target_totaal=sum(target_totaal),
            target_t1=sum(target_t1))

openxlsx::write.xlsx(steekproefkader, "04 output tabellen/steekproefkader24.xlsx")

saveRDS(steekproefkader, file = "04 output tabellen/steekproefkader24.rds")

