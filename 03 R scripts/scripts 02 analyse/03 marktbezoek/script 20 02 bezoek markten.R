
library(tidyverse)


data_markt_def <- read_rds("03 tussentijds/data_markt_def.RDS")

data_markt_def$data_22 <- data_markt_def$data_22 |>
  mutate(weeg_ams=case_when(
    is.na(wg) ~ 1,
    TRUE ~ wg)
  )

data_markt_def$data_20 <- data_markt_def$data_20 |>
  mutate(weeg_ams=case_when(
    is.na(wstad) ~ 1,
    TRUE ~ wstad)
  )

jaren<- c("monitor 2024", "monitor 2022", "monitor 2020")

data_markt_def <- data_markt_def |>
  map2(jaren, \(x,y) add_column(x, monitor= y ))|>
  bind_rows()





# frequentietabellen

my_table<- function (x, marktvar=marktbezoek, groupvars) {
  
  x |>
    group_by(monitor, {{marktvar}}, {{groupvars}}, .drop = F)|>
    summarise(
      aantal=n(),
      aantal_gew= sum(weeg_ams, na.rm = T))|>
    
    group_by(monitor,{{groupvars}})|>
    
    mutate(
      aandeel=aantal/sum(aantal)*100,
      aandeel_gew=aantal_gew/sum(aantal_gew)*100 )
  
}



lev_std <- c("Centrum","Westpoort","West" , "Nieuw-West","Zuid" ,"Oost" , 
             "Noord","Weesp",  "Zuidoost" ,  NA )

lev_geb <- c('Centrum', 'Zuid, West Oost', 'NW, Noord, ZO, Weesp')

lev_ink <- c(
  "inkomen laag",
  "inkomen midden",
  "inkomen hoog",
  "inkomen onbekend")

lev_opl <- c(
  "praktisch opgeleid","middelbaar opgeleid",
  "theoretisch opgeleid", "opleiding onbekend")

lev_lft <- c(
  "35 jaar of jonger",
  "35 jaar tot en met 55 jaar",
  "55 jaar of ouder",
  "onbekend")


lev_v13 <- c(
  "een aantal keer per week",  
  "1 keer per week"  ,  
  "1 keer per twee weken" , 
  "1 keer per maand" ,  
  "minder dan 1 keer per maand" ,   
  "zelden tot nooit",
  "weet niet, geen antwoord") 




my_list <- function (x, marktvar) {
  
  list(
    
    # totaal stad
    tab_v1_ams = x |> 
      group_by(monitor, {{marktvar}})|>
      summarise(aantal=n(),
                aantal_gew= sum(weeg_ams, na.rm = T))|>
      
      group_by(monitor)|>
      mutate(aandeel=aantal/sum(aantal)*100,
             aandeel_gew=aantal_gew/sum(aantal_gew)*100),
    
    # per stadsdeel 
    tab_v1_sd = x |> 
      my_table({{marktvar}}, 
               groupvars = across(all_of(c("gbd_sdl_code", "gbd_sdl_naam"))))|>
      mutate(gbd_sdl_naam=factor(gbd_sdl_naam, levels = lev_std))|>
      add_column(inkomen_klas="totaal"),
    
    # per wijk
    tab_v1_wijk = x |> 
      my_table({{marktvar}}, 
               groupvars = across(all_of(c("gbd_wijk_code", "gbd_wijk_naam", "gbd_sdl_code", "gbd_sdl_naam"))))|>
      add_column(inkomen_klas="totaal"),
    
    # per gebied
    tab_v1_geb = x |> 
      my_table({{marktvar}}, 
               groupvars = across(all_of(c("gbd_ggw_code", "gbd_ggw_naam", "gbd_sdl_code", "gbd_sdl_naam"))))|>
      add_column(inkomen_klas="totaal"),
    
    
    # per inkomensgroep 
    tab_v1_ink = x |>
      my_table({{marktvar}}, inkomen_klas)|>
      mutate(inkomen_klas=factor(inkomen_klas, levels = lev_ink)),
    
    # per opleidingsgroep 
    tab_v1_opl = x |>
      my_table({{marktvar}}, opleiding_klas)|>
      mutate(opleiding_klas=factor(opleiding_klas, levels = lev_opl)),
    
    # per huishoudgroep 
    tab_v1_hhsam = x |>
      my_table({{marktvar}},huishouden_klas),
    
    # per leeftijdsgroep
    tab_v1_leefklas = x |>
      my_table({{marktvar}}, leeftijd_klas)|>
      mutate(leeftijd_klas=factor(leeftijd_klas, levels = lev_lft)),
    
    # inkomen en sd
    tab_v1_sd_ink = bind_rows(
      
      x |>
        my_table({{marktvar}}, groupvars = across(all_of(c("inkomen_klas", "gbd_sdl_naam"))))|>
        mutate(inkomen_klas=factor(inkomen_klas, levels = lev_ink))|>
        mutate(gbd_sdl_naam=factor(gbd_sdl_naam, levels = lev_std)),
      
      x |> 
        my_table({{marktvar}}, groupvars = across(all_of(c("gbd_sdl_code", "gbd_sdl_naam"))))|>
        mutate(gbd_sdl_naam=factor(gbd_sdl_naam, levels = lev_std))|>
        add_column(inkomen_klas="totaal")
    ),
    
    # leeftijd en sd
    tab_v1_sd_lft = bind_rows(
      
      x |>
        my_table({{marktvar}}, groupvars = across(all_of(c("leeftijd_klas", "gbd_sdl_naam"))))|>
        mutate(leeftijd_klas=factor(leeftijd_klas, levels = lev_lft))|>
        mutate(gbd_sdl_naam=factor(gbd_sdl_naam, levels = lev_std)),
      
      x |> 
        my_table({{marktvar}}, groupvars = across(all_of(c("gbd_sdl_code", "gbd_sdl_naam"))))|>
        mutate(gbd_sdl_naam=factor(gbd_sdl_naam, levels = lev_std))|>
        add_column(inkomen_klas="totaal")
    )
  )
}

# geen marktbezoek
tabel_list_geenmarkt <- data_markt_def |>
  my_list(marktvar = marktbezoek)

# bezoekfrequentie
tabel_list_marktfreq <- data_markt_def |>
  my_list(marktvar = v13)|>
  map(\(x) mutate(x, v13=factor(v13, levels = lev_v13)))

# meest bezochte markt
tabel_list_marktnaam<- data_markt_def |>
  my_list(marktvar = v15_schoon)

openxlsx:: write.xlsx(tabel_list_geenmarkt, "04 output tabellen/tabellen_geenmarkt.xlsx", withFilter=T, overwrite = T)
openxlsx:: write.xlsx(tabel_list_marktfreq, "04 output tabellen/tabellen_marktfreq.xlsx", withFilter=T, overwrite = T)
openxlsx:: write.xlsx(tabel_list_marktnaam, "04 output tabellen/tabellen_markt.xlsx", withFilter=T, overwrite = T)



save( tabel_list_geenmarkt, tabel_list_marktfreq, tabel_list_marktnaam, file= "03 tussentijds/tabellen_markten_def.RData")



my_select<- function(x) {
  
  x |>
    select(-c("aantal", "aantal_gew", "aandeel"))
}

tabel_list_marktfreq <- tabel_list_marktfreq |>
  map(\(x) my_select(x))
tabel_list_marktfreq["tab_v1_wijk"]<- NULL  

tabel_list_marktnaam <- tabel_list_marktnaam |>
  map(\(x) my_select(x))
tabel_list_marktnaam["tab_v1_wijk"]<- NULL  

openxlsx:: write.xlsx(tabel_list_marktfreq, "04 output tabellen/tab_mondet24_martkfreq.xlsx", withFilter=T, overwrite = T)
openxlsx:: write.xlsx(tabel_list_marktnaam, "04 output tabellen/tab_mondet24_martknaam.xlsx", withFilter=T, overwrite = T)

  
  
  
  
  


