
########################
# achtergrondvartiabelen
########################

# opleiding-

opl<- list()

opl$theoretisch<- c (
  "WO, universiteit, kandidaatsexamen",
  "hoger beroepsonderwijs (hbo), associate degree, hbo-bachelor, wo-bachelor",
  "wetenschappelijk onderwijs/universiteit/master, hbo-master, wo-master , PhD",
  "HBO, interne opleiding op hbo-niveau") 

opl$mbo <- c (
  "vwo, gymnasium, atheneum, hbs", 
  "MBO-lang, interne opleiding op mbo-niveau, BBL/BOL 3-4",   
  "HAVO, VWO, HBS, MMS",
  "havo, mms",
  "middelbaar beroepsonderwijs (mbo 2, 3 of 4)")

opl$praktisch <- c(
  "MAVO, MULO, VMBO", 
  "mavo, mulo, ulo", 
  "VBO/LBO (huishoud-, ambachtsschool, LTS, interne bedrijfsopleiding), MBO-kort, BBL/BOL 1-2, leerlingwezen, ULO",
  "geen opleiding gevolgd of enkele jaren lagere school, basisschool gevolgd",
  "geen opleiding afgerond",
  "lagere school, basisschool, speciaal onderwijs",
  "VSO, voortgezet speciaal onderwijs",
  "lagere school / basisschool, speciaal (basis)onderwijs",                  
  "lager beroepsonderwijs (lbo, vbo, vso, mbo niveau 1, praktijkonderwijs)",
  "vmbo")

opl$onbekend <- c(
  "weet niet",
  "anders, namelijk", 
  "opleiding in buitenland gevolgd",                                                                                                  
  "geen antwoord",                                                                           
  "anders")

# inkomen

ink<- list()

ink$laag <- c(
  "netto 1.000 euro per maand of minder",
  "netto tussen de 1.001 en 1.350 euro per maand",
  "netto tussen de 1.351 en 1.750 euro per maand",
  "minder dan €1000", 
  "tussen €1001 en €1350",
  "tussen €1351 en €1750")

ink$midden<- c(
  "netto tussen de 1.751 en 3.050 euro per maand",
  "netto tussen de 3.051 en 4.000 euro per maand",
  "netto tussen de 4.001 en 5.000 euro per maand",
  "tussen €1751 en €2400",
  "tussen €2401 en €3050",
  "tussen €3051 en €4000",
  "tussen €4001 en €5000")

ink$hoog<- c(
  "netto tussen de 5.001 en 6.000 euro per maand",
  "netto meer dan 6.000 euro per maand", 
  "tussen €5001 en €6000",
  "meer dan €6001")

ink$onbekend<- c(10,
  "weet niet, geen antwoord")


# huishoudtype - 

huish<- list()

huish$alleen<- c(
  "alleenstaand",
  "een persoon, alleenstaande")

huish$stel <- c(
  "(echt)paar zonder thuiswonend(e) kind(eren)",
  "(echt)paar zonder kinderen thuis")
 
 huish$kinderen <- c(
 "anders, met thuiswonend(e) kind(eren)",
 "anders, met thuiswonend(e) kind(eren)",
 "(echt)paar met kind(eren) thuis",
 "(echt)paar met thuiswonend(e) kind(eren)")
 
huish$eenouder <- c(
  "één-ouder met thuiswonend(e) kind(eren)",
  "een ouder met kind(eren) thuis")
 
huish$overig<- c(
  "studentenhuis", 
  "met huisgenoot, geen studentenwoning", 
  "anders, zonder thuiswonend(e) kind(eren)",
  "anders, zonder thuiswonende kinderen",
  "geen antwoord", 
  "weet niet")


### functies voor data 2024 ---
my_opl24 <- function(x){
  
  x |> mutate(
    
    OPLEID= str_trim(OPLEID, "both"),
    OPLEID_panel= str_trim(OPLEID_panel, "both"),
    
    opleiding_klas=
      case_when(
        OPLEID %in% opl$praktisch ~ "praktisch opgeleid",
        OPLEID %in% opl$mbo ~ 'middelbaar opgeleid',
        OPLEID %in% opl$theoretisch ~ "theoretisch opgeleid",
        OPLEID %in% opl$onbekend ~ "opleiding onbekend",
        
        OPLEID_panel %in% opl$praktisch ~ "praktisch opgeleid",
        OPLEID_panel %in% opl$mbo ~ 'middelbaar opgeleid',
        OPLEID_panel %in% opl$theoretisch ~ "theoretisch opgeleid",
        OPLEID_panel %in% opl$onbekend ~ "opleiding onbekend",
        
        (is.na(OPLEID) & is.na(OPLEID_panel)) ~ "opleiding onbekend")
  )
}

my_inkomen24 <- function(x){
  
  x |> mutate(
    
    INKOMEN= str_trim(INKOMEN, "both"),
    INKOMEN_panel= str_trim(INKOMEN_panel, "both"),
    
    
    inkomen_klas=case_when(
      INKOMEN %in% ink$laag     ~ "inkomen laag",
      INKOMEN %in% ink$midden   ~ "inkomen midden",
      INKOMEN %in% ink$hoog     ~ "inkomen hoog",
      INKOMEN %in% ink$onbekend ~ "inkomen onbekend",
      
      INKOMEN_panel %in% ink$laag     ~ "inkomen laag",
      INKOMEN_panel %in% ink$midden   ~ "inkomen midden",
      INKOMEN_panel %in% ink$hoog     ~ "inkomen hoog",
      INKOMEN_panel %in% ink$onbekend ~ "inkomen onbekend",
      
      (is.na(INKOMEN) & is.na(INKOMEN_panel)) ~ "inkomen onbekend")
  )
  
}

my_gesl24 <- function(x) {
  
  
  x |> mutate(
    
    GESL= str_trim(GESL, "both"),
    
    geslacht=case_when(
      GESL == 'man' ~ 'man',
      GESL == 'vrouw' ~ 'vrouw',
      TRUE ~ 'onbekend')
  )
}

my_huish24 <- function (x) {
  
  x |>
    mutate(
      
      HHSAM= str_trim(HHSAM, "both"),
      HHSAM_panel= str_trim(HHSAM_panel, "both"),
      
      huishouden_klas=case_when(
        HHSAM %in% huish$alleen   ~ "een persoon",
        HHSAM %in% huish$stel     ~ "paar zonder kinderen",
        HHSAM %in% huish$kinderen ~ "paar met kinderen",
        HHSAM %in% huish$eenouder ~ "een ouder",
        HHSAM %in% huish$overig   ~ "overig, of huishoudtype onbekend",
        
        HHSAM_panel %in% huish$alleen   ~ "een persoon",
        HHSAM_panel %in% huish$stel     ~ "paar zonder kinderen",
        HHSAM_panel %in% huish$kinderen ~ "paar met kinderen",
        HHSAM_panel %in% huish$eenouder ~ "een ouder",
        HHSAM_panel %in% huish$overig   ~ "overig, of huishoudtype onbekend",
        
        (is.na(HHSAM) & is.na(HHSAM_panel)) ~ "overig, of huishoudtype onbekend")
    )
}

my_leeftijd24 <- function(x){
  
  x |>
    mutate(
      leeftijd_klas=case_when(
        LEEFTD < 35 ~"35 jaar of jonger",
        LEEFTD %in% c(35:55) ~ "35 jaar tot en met 55 jaar",
        LEEFTD > 55 ~"55 jaar of ouder",
        TRUE ~ "onbekend")
    )
}

## def herkomst functie
my_herkomst24 <- function (x) {
  
  x|>
    my_opl24()|>
    my_gesl24()|>
    my_inkomen24()|>
    my_huish24()|>
    my_leeftijd24()
  
  
}

#######################################
### functies voor data 2020 en 2022 ---
#######################################

my_opl22 <- function(x){
  
  x |> mutate(
    
    opleid= str_trim(opleid, "both"),
    
    opleiding_klas=
      case_when(
        opleid %in% opl$praktisch ~ "praktisch opgeleid",
        opleid %in% opl$mbo ~ 'middelbaar opgeleid',
        opleid %in% opl$theoretisch ~ "theoretisch opgeleid",
        opleid %in% opl$onbekend ~ "opleiding onbekend",
        is.na(opleid) ~ "opleiding onbekend")
  )
}

my_inkomen22 <- function(x){
  
  x |> mutate(
    
    inkomen= str_trim(inkomen, "both"),
    inkomen_klas=case_when(
      inkomen %in% ink$laag     ~ "inkomen laag",
      inkomen %in% ink$midden   ~ "inkomen midden",
      inkomen %in% ink$hoog     ~ "inkomen hoog",
      inkomen %in% ink$onbekend ~ "inkomen onbekend",
      is.na(inkomen)            ~ "inkomen onbekend")
  )
  
}

my_gesl22 <- function(x) {
  
  
  x |> mutate(
    
    gesl= str_trim(gesl, "both"),
    
    geslacht=case_when(
      gesl == 'man' ~ 'man',
      gesl == 'vrouw' ~ 'vrouw',
      TRUE ~ 'onbekend')
  )
}

my_huish22<- function (x) {
  
  x |>
    mutate(
      
      hhsam= str_trim(hhsam, "both"),
      
      huishouden_klas=case_when(
        hhsam %in% huish$alleen   ~ "een persoon",
        hhsam %in% huish$stel     ~ "paar zonder kinderen",
        hhsam %in% huish$kinderen ~ "paar met kinderen",
        hhsam %in% huish$eenouder ~ "een ouder",
        hhsam %in% huish$overig   ~ "overig, of huishoudtype onbekend",
        is.na(hhsam)              ~ "overig, of huishoudtype onbekend")
    )
}

my_leeftijd22<- function(x){
  
  x |>
    mutate(
      leeftijd_klas=case_when(
        leeftd < 35 ~"35 jaar of jonger",
        leeftd %in% c(35:55) ~ "35 jaar tot en met 55 jaar",
        leeftd > 55 ~"55 jaar of ouder",
        TRUE ~ "onbekend")
    )
}

## def herkomst functie
my_herkomst22 <- function (x) {
  
  x|>
    my_opl22()|>
    my_gesl22()|>
    my_inkomen22()|>
    my_huish22()|>
    my_leeftijd22()
  
  
}


#############################################
##### winkelgebieden per productgroep NDG ---
#############################################


pdg<- list()

pdg$code <- c(
  
  "V18_nw_modeartikelen_GV1",
  "V18_nw_elektronica_GV1",
  "V18_nw_huishoudelijk_GV1",
  "V18_nw_woninginrichting_GV1",
  "V18_nw_doehetzelf_GV1",
  "V18_nw_planten_GV1",  
  "V18_nw_media_GV1",  
  "V18_nw_sportspel_GV1")

pdg$naam <- c(
  
  "modeartikelen",
  "elektronica",
  "huishoudelijke artikelen",
  "woninginrichting",
  "doe het zelf",
  "planten en bloemen",  
  "media en vrijetijd",  
  "sport en spel")


# andere aanschaflocaties NDG -
pdg$anders <- c(
  
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

pdg$omzetcijfers <- c(0.188,
                      0.084,
                      0.171,
                      0.258,
                      0.043,
                      0.050,
                      0.080,
                      0.032)

df_omzetcijfers <- tibble(pdg$code, pdg$naam, pdg$omzetcijfers)
                        
