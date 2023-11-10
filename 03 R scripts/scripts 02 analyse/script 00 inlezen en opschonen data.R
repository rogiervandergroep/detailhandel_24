

# lees libraries in met setup
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# inlezen data met weegfactoren 
# uit "03 R scripts/scripts 01 weging en respons/script 05 wegingsfactoren.R"



load(file = "03 tussentijds/data_24_DEF.RDS")




  


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



data_24_weeg_herk<- data_24_weeg %>%
  select(Respondent_ID, any_of(herkomst_var))%>%
  haven::as_factor()

data_24_weeg_herk <- data_24_weeg_herk %>%
  my_herkomst()%>%
  select (Respondent_ID, any_of(herkomst_var_nieuw))

data_24_weeg<- data_24_weeg%>%
  left_join(data_24_weeg_herk, by = "Respondent_ID")

##### winkelgebieden per productgroep NDG ----
pdg_code <- c(
  
  "V18_nw_modeartikelen_GV1",
  "V18_nw_elektronica_GV1",
  "V18_nw_huishoudelijk_GV1",
  "V18_nw_woninginrichting_GV1",
  "V18_nw_doehetzelf_GV1",
  "V18_nw_planten_GV1",  
  "V18_nw_media_GV1",  
  "V18_nw_sportspel_GV1")

pdg_naam <- c(
  
  "modeartikelen",
  "elektronica",
  "huishoudelijke artikelen",
  "woninginrichting",
  "doe het zelf",
  "planten en bloemen",  
  "media en vrijetijd",  
  "sport en spel")


# andere aanschaflocaties NDG -
pdg_anders <- c(
  
  "V18_nw_modeartikelen_GV1_Codes",                        
  "V18_nw_elektronica_GV1_Codes",                       
  "V18_nw_huishoudelijk_GV1_Codes",  
  "V18_nw_woninginrichting_GV1_Codes",  
  "V18_nw_doehetzelf_GV1_Codes",    
  "V18_nw_planten_GV1_Codes",  
  "V18_nw_media_GV1_Codes", 
  "V18_nw_sportspel_GV1_Codes")

# V18_nw_modeartikelen_GV1		0,188
# V18_nw_elektronica_GV1		  0,084
# V18_nw_huishoudelijk_GV1		0,171
# V18_nw_woninginrichting_GV1	0,258
# V18_nw_doehetzelf_GV1	      0,043
# V18_nw_planten_GV1		      0,050
# V18_nw_media_GV1		        0,080
# V18_nw_sportspel_GV1		    0,032

omzetcijfers <- c(0.188,
                  0.084,
                  0.171,
                  0.258,
                  0.043,
                  0.050,
                  0.080,
                  0.032)

df_omzetcijfers <- tibble(pdg_code, pdg_naam, omzetcijfers)






save(data_24_weeg, pdg_code, pdg_naam, pdg_anders, omzetcijfers, df_omzetcijfers, file= "03 tussentijds/data_24_DEF.Rdata")
