library(tidyverse)
library(openxlsx)
library(haven)


source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")



# inwoners per wijk 2023
wijk_inw22 <- dbGetQuery(
  con = db_con_ref2(path =  pad),
  "
 SELECT jaar, gebiedcode_15, waarde, indicator_definitie_id
 FROM public.bbga_kerncijfers
 WHERE (LENGTH(gebiedcode_15) <= 2 or gebiedcode_15 LIKE 'G%' or gebiedcode_15 = 'STAD') AND 
 jaar = 2023 AND 
 indicator_definitie_id = 'BEVTOTAAL';

  ") |>
  rename(gbd_wijk_code = gebiedcode_15,
         inwoners_22 = waarde) |>
  select(jaar:inwoners_22)

# koppelen gebiedsnamen, stadsdelen

gebiedsindeling_buurt <- read.csv2("02 lookup tabellen/gebiedsindeling2022.csv")
gebiedsindeling_wijk <- gebiedsindeling_buurt |>
  select (gbd_wijk_code:gbd_sdl_naam) |>
  distinct(gbd_wijk_code, .keep_all = T) |>
  mutate(gbd_wijk_code =case_when(is.na(gbd_wijk_code) ~ 'NA',
                                  TRUE ~ gbd_wijk_code))
         

wijk_inw22_def <- wijk_inw22 |>
  filter(nchar(gbd_wijk_code)==2) |>
  left_join(gebiedsindeling_wijk, by=c("gbd_wijk_code")) |>
  select(gbd_wijk_code, gbd_wijk_naam:gbd_sdl_naam, inwoners_22)







# inlezen inkomen per wijK 
# bron https://www.cbs.nl/nl-nl/maatwerk/2023/35/inkomen-per-gemeente-en-wijk-2020
# gemiddeld inkomen Amsterdam in 2020	32,4

wijk_ink <- read.xlsx(
  "02 lookup tabellen/besteed_gini_2021 vor hetty.xlsx", sheet="cbs_ink_wijk", )|>
  mutate(gbd_wijk_code=str_sub(wijk_code_cbs, 7,8)) |>
  select(gbd_wijk_code, ink_wijk)|>
  add_column(ink_ams = 32.4) |>
  mutate(ink_coef = case_when(ink_wijk != 0 ~  ink_wijk/ink_ams,
                              ink_wijk == 0 ~   NA))

wijk_inw22_ink22 <- wijk_inw22_def |>
  left_join(wijk_ink, by = "gbd_wijk_code")|>

  mutate(inw22_aandeel_ams = (inwoners_22/sum(inwoners_22, na.rm=T)*100)) |>
  group_by(gbd_sdl_code)|>
  mutate(inw22_aandeel_sd =  (inwoners_22/sum(inwoners_22, na.rm=T)*100))  |>
  group_by(gbd_ggw_code)|>
  mutate(inw22_aandeel_geb = (inwoners_22/sum(inwoners_22, na.rm=T)*100)) |>
  ungroup()

# bereken de respons van data 24
data_24 <- read_sav("00 ruwe data 2022 2023/230349_TOTAAL_2022_2023.sav")




# herkomstvelden opschonen

data_24 <- data_24 |>
  mutate(gbd_wijk_code=case_when(
    gbd_wijk_code=='' ~ bctk22,
    TRUE ~ gbd_wijk_code),
    
    gbd_wijk_naam=case_when(
      gbd_wijk_naam=='' ~ bctk22n,
      TRUE ~ gbd_wijk_naam),
    
    gbd_ggw_code=case_when(
      gbd_ggw_code=='' ~ i25geb,
      TRUE ~ gbd_ggw_code),
    
    gbd_sdl_code=case_when(
      gbd_sdl_code=='' ~ stad22,
      TRUE ~ gbd_sdl_code)
  )


# toevoegen gebiedscode aan afzet_gebied en afzet_sd
gebiedsindeling_buurt <- read.csv2("02 lookup tabellen/gebiedsindeling2022.csv")

gebiedsindeling_sd <- gebiedsindeling_buurt |>
  select (gbd_sdl_code:gbd_sdl_naam) |>
  distinct(gbd_sdl_code, .keep_all = T) 

gebiedsindeling_gebied <- gebiedsindeling_buurt |>
  select (gbd_ggw_code:gbd_ggw_naam) |>
  distinct(gbd_ggw_code, .keep_all = T)        

data_24j <- data_24 |>
  select(-c(gbd_sdl_naam,gbd_ggw_naam)) |>
  left_join(gebiedsindeling_sd,     by = "gbd_sdl_code")|>
  left_join(gebiedsindeling_gebied, by= "gbd_ggw_code")

data_24 <- data_24j |>
  select(-c(stad15,i22geb,bctk22,bctk22n,i25geb,stad15,i22geb, 
            stad22,cbscode,altgeb_code,altgeb_naam))





data_repons <- list()

data_repons$sd  <- data_24 |>
  group_by(gbd_sdl_code)|>
  summarise(respons=n())

data_repons$geb  <- data_24 |>
  group_by(gbd_ggw_code)|>
  summarise(respons=n())

data_repons$wijk  <- data_24 |>
  group_by(gbd_wijk_code)|>
  summarise(respons=n())

# koppelen respons per wijk aan inwoners wijk en aandelen amsterdam, geb en sd

wijk_resp <- wijk_inw22_ink22 |>
  left_join(data_repons$wijk, by= c("gbd_wijk_code")) |>
  
  ungroup() |>
 
  # koppelen respons per wijk aan inwoners wijk en aandelen amsterdam, geb en sd

  mutate(resp_aandeel_ams = respons/sum(respons, na.rm=T)*100) |>
  group_by(gbd_sdl_code)|>
  mutate(resp_aandeel_sd  =  respons/sum(respons, na.rm=T)*100) |>
  group_by(gbd_ggw_code)|>
  mutate(resp_aandeel_geb = respons/sum(respons, na.rm=T)*100)  |>
  
  # toevoegen wegingsfactoren
  
  ungroup() |> 
  
  # gewicht op inwoners per wijk
  mutate(
    weeg_ams  = case_when(
      is.na (inw22_aandeel_ams) ~ 1,
      is.na (resp_aandeel_ams)  ~ 1,
      !is.na(inw22_aandeel_ams) ~ inw22_aandeel_ams/resp_aandeel_ams,
      !is.na(inw22_aandeel_ams) ~ inw22_aandeel_ams/resp_aandeel_ams),
    
    weeg_sd  = case_when(
      is.na (inw22_aandeel_sd) ~ 1,
      is.na (resp_aandeel_sd)  ~ 1,
      !is.na(inw22_aandeel_sd) ~ inw22_aandeel_sd/resp_aandeel_sd,
      !is.na(inw22_aandeel_sd) ~ inw22_aandeel_sd/resp_aandeel_sd),
    
    weeg_geb = case_when(
      is.na (inw22_aandeel_geb) ~ 1,
      is.na (resp_aandeel_geb)  ~ 1,
      !is.na(inw22_aandeel_geb) ~ inw22_aandeel_geb/resp_aandeel_geb,
      !is.na(inw22_aandeel_geb) ~ inw22_aandeel_geb/resp_aandeel_geb),
  
  
  # gewicht op inwoners per wijk en inkomen per wijk 
    weeg_ams_ink  = case_when(
      is.na (ink_coef) ~ weeg_ams,
      !is.na(ink_coef) ~ weeg_ams*ink_coef),
                               
    weeg_sd_ink   = case_when(
      is.na (ink_coef) ~ weeg_sd,
      !is.na(ink_coef) ~ weeg_sd*ink_coef),
                                     
         
    weeg_geb_ink  = case_when(
      is.na (ink_coef) ~ weeg_geb,
      !is.na(ink_coef) ~ weeg_geb*ink_coef))


# to do: inkomen op sd niveau


wijk_resp_kort <- wijk_resp |>
  select(gbd_wijk_code, inwoners_22:weeg_geb_ink)



# toevoegen weegfactoren aan dataset 
data_24_weeg <- data_24 |>
  left_join(wijk_resp_kort, by= c("gbd_wijk_code")) 

save(data_24_weeg, file = "03 tussentijds/data_24_DEF.RDS")


selectie_weesp <- data_24_weeg |>
  filter(gbd_sdl_naam == 'Weesp')


  
  
  



  


        