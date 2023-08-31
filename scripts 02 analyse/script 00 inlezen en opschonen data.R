library(tidyverse)
library(haven)

# lees libraries in met setup
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# inlezen data
### toevoegen tijdelijke data 2022 2023
datadef24_1 <- read_spss("00 ruwe data 2022 2023/230349_1_gecodeerd.sav")
datadef24_2 <- read_spss("00 ruwe data 2022 2023/230349_2_gecodeerd.sav")
datadef24_3 <- read_spss("00 ruwe data 2022 2023/230349_3_tussentijds.sav")

datadef24 <- bind_rows(datadef24_1, datadef24_2, datadef24_3)
rm(datadef24_1, datadef24_2, datadef24_3)

# inlezen respons
df_inw <- read.xlsx("02 lookup tabellen/22015 basis voor steekproeftrekking 2022 2023 steek 2.xlsx")%>%
  mutate(inw_aandeel = inwoners22/sum(inwoners22),
         resp_aandeel= respons_t1/sum(respons_t1),
         weegfactor  = case_when(resp_aandeel == 0 ~ 1,
                                 TRUE ~ (inw_aandeel/resp_aandeel)))

## to do : weegfactor gemiddeld inkomen naar stadsdeel

# stap 1 inlezen weegfactor per stadsdeel

# stap 2 berekening nieuwe weegfactor gebieds_weeegfactor te vermenigvuldigen met inkomen


# weegfactor om wijken met lage responds op te hogen en vv
datadef24<-datadef24 |>
  left_join(df_inw[, c("wijk_code", "weegfactor")], 
            by=c("gbd_wijk_code"="wijk_code"))


# hercoderen achtergrondvars

my_opl <- function(x){
  
  x %>% mutate(opleiding=
                 case_when(OPLEID %in% c("lagere school, basisschool, speciaal onderwijs", 
                                         "geen opleiding gevolgd of enkele jaren lagere school, basisschool gevolgd",
                                         "VBO/LBO (huishoud-, ambachtsschool, LTS, interne bedrijfsopleiding), MBO-kort, BBL/BOL 1-2, leerlingwezen, ULO") ~ "praktisch opgeleid",
                           OPLEID %in% c("MAVO, MULO, VMBO",
                                         "MBO-lang, interne opleiding op mbo-niveau, BBL/BOL 3-4",
                                         "HAVO, VWO, HBS, MMS") ~ 'middelbaar opgeleid',
                           OPLEID %in% c("WO, universiteit, kandidaatsexamen",
                                         "HBO, interne opleiding op hbo-niveau") ~  "theoretisch opgeleid",
                           TRUE ~ 'onbekend'))
  
}

my_inkomen <- function(x){
  
  x %>% mutate(inkomen=
                 case_when(INKOMEN %in% c("netto 1.000 euro per maand of minder", 
                                          "netto tussen de 1.001 en 1.350 euro per maand",
                                          "netto tussen de 1.351 en 1.750 euro per maand") ~"inkomen laag",
                           INKOMEN %in% c("netto tussen de 1.751 en 3.050 euro per maand",
                                          "netto tussen de 3.051 en 4.000 euro per maand",
                                          "netto tussen de 4.001 en 5.000 euro per maand") ~ "inkomen midden",
                           INKOMEN %in% c("netto tussen de 5.001 en 6.000 euro per maand",
                                          "netto meer dan 6.000 euro per maand")           ~"inkomen hoog",
                           TRUE ~ 'weet niet, geen antwoord'))
  
}

my_gesl <- function(x) {
  
  
  x %>% mutate(geslacht=
                 case_when(GESL == 'man' ~ 'man',
                           GESL == 'vrouw' ~ 'vrouw',
                           TRUE ~ 'onbekend'))
  
} 

my_huish<- function (x) {
  
  x %>% 
    mutate(
      huishouden=
             case_when(HHSAM == "een persoon, alleenstaande" ~ "een persoon",
                       HHSAM == "(echt)paar zonder kinderen thuis)" ~ "paar zonder kinderen",
                       HHSAM == "(echt)paar met kind(eren) thuis"~ "paar met kinderen",
                       HHSAM == "een ouder met kind(eren) thuis"~ "een ouder",
                       TRUE ~ "overig, onbekend"))

  
}

my_leeftijd<- function(x){
  
  x %>% 
    mutate(
      leeftijdklas=
        case_when(LEEFTD < 35 ~"35 jaar of jonger",
                  LEEFTD %in% c(35:55) ~ "35 jaar tot en met 55 jaar",
                  LEEFTD > 55 ~"55 jaar of ouder",
                  TRUE ~ "onbekend"))
  
}

## def herkomst functie
my_herkomst <- function (x) {
  
  x%>%
    my_opl()%>%
    my_gesl()%>%
    my_inkomen()%>%
    my_huish()%>%
    my_leeftijd

  
}

herkomst_var <- c("OPLEID", "INKOMEN", "HHSAM", "GESL", "LEEFTD")
herkomst_var_nieuw <- c("opleiding", "inkomen", "huishouden", "geslacht", "leeftijdklas")

datadef24$Respondent_ID<- str_pad(
  datadef24$Respondent_ID, 
  width=5, 
  pad="0", 
  side= "left")

datadef24_herk<- datadef24 %>%
  select(Respondent_ID, any_of(herkomst_var))%>%
  haven::as_factor()

datadef24_herk <- datadef24_herk %>%
  my_herkomst()%>%
  select (Respondent_ID, any_of(herkomst_var_nieuw))

datadef24<- datadef24%>%
  left_join(datadef24_herk, by = "Respondent_ID")

saveRDS(datadef24, file= "02 lookup tabellen/werkbestand consumenten enquete.rds")
