

# lees libraries in met setup
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# inlezen data met weegfactoren 
# uit "03 R scripts/scripts 01 weging en respons/script 05 wegingsfactoren.R"



load(file = "03 tussentijds/data_24_DEF.Rdata")

"V4_nw" = "winkelgebied waar men normaliter boodschappen doet"
"V20"   = "winkelgebied om te winkelen"

rap_sel <- c()

rap_v7 <- c(
"V6",
"V7_nw_het_uiterlijk_va_GV1",              
"V7_nw_de_aankleding_en_GV1",              
"V7_nw_de_sfeer_en_de_g_GV1",              
"V7_nw_de_keuze_mogelij_GV1",              
"V7_nw_bio_aanbod_GV1" ,                   
"V7_nw_de_keuze_mogeli6_GV1",              
"V7_nw_duurzaam_aanbod_GV1" ,              
"V7_nw_het_algemeen_pri_GV1" ,             
"V7_nw_veiligheid_GV1" ,                   
"V7_nw_veiligheid_avond_GV1",              
"V7_nw_schoonhouden_GV1",                  
"V7_nw_overlast_GV1",                      
"V7_nw_overlast_vuil_GV1" ,                
"V7_nw_overlast_hor_GV1" ,                 
"V7_nw_parkerenfiets_GV1",                 
"V7_nw_parkerenauto_GV1",                  
"V7_nw_aanbod_daghoreca_GV1"  ,            
"V7_nw_bereikbaar_GV1")

rap_v22 <- c(
"V21",
"V22_nw_het_uiterlijk_va_GV1" ,            
"V22_nw_de_aankleding_en_GV1",             
"V22_nw_de_sfeer_en_de_g_GV1" ,            
"V22_nw_de_keuze_mogelij_GV1",             
"V22_nw_bio_GV1"  ,                        
"V22_nw_de_keuze_mogeli6_GV1" ,            
"V22_nw_duurzame_GV1"   ,                  
"V22_nw_het_algemeen_pri_GV1" ,            
"V22_nw_veiligheid_GV1"     ,              
"V22_nw_veiligheid_avond_GV1" ,            
"V22_nw_schoonhouden_GV1",                 
"V22_nw_overlast_GV1",                     
"V22_nw_overlast_vuil_GV1" ,               
"V22_nw_overlast_horeca_GV1" ,             
"V22_nw_parkerenfiets_GV1" ,               
"V22_nw_parkerenauto_GV1",                 
"V22_nw_aanbod_daghoreca_GV1" ,            
"V22_nw_bereikbaar_GV1")

  
df_rap_dg <-  data_24_weeg |>
  select(any_of(c("Respondent_ID",  "weeg_ams_ink", "V4_nw", rap_v7))) |>
  add_column(productgroep="winkelgebied voor dagelijkse boodschappen") |>
  filter(!is.na(V4_nw))

df_rap_ndg <- data_24_weeg |>
  select(any_of(c("Respondent_ID",  "weeg_ams_ink", "V20", rap_v22)))|>
  add_column(productgroep="winkelgebied voor niet-dagelijkse boodschappen")|>
  filter(!is.na(V20))



labels_dg <- df_rap_dg |>
  names()|>
  map_df(
    \(i) tibble(labels = attr(df_rap_dg[[i]], "label"), 
                name = i))|>
  mutate(labels = str_replace_all(labels, ":.*", ""),
         labels = str_replace_all(labels, "[']", ""),
         labels = case_when(
           name == "V4_nw" ~ "In welk winkelgebied doet u normaliter de boodschappen?",
           name == "V6" ~ "Wat is uw totaaloordeel over dit winkelgebied?",
           TRUE ~ labels))


labels_ndg <- df_rap_ndg |>
  names()|>
  map_df(
    \(i) tibble(
      labels = attr(df_rap_ndg[[i]], "label"), 
      name = i))|>
  mutate(labels = str_replace_all(labels, ":.*", ""),
         labels = str_replace_all(labels, "[']", ""),
         labels = case_when(
           name == "V20" ~ "Wat is uw favoriete winkelgebied in Amsterdam of de regio om te winkelen voor uw plezier?",
           name == "V21" ~ "Wat is uw totaaloordeel over dit winkelgebied?",
           TRUE ~ labels))



gem_rap  <- bind_rows(
  
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7) ) |>
    left_join(labels_dg, by= "name") |>
    group_by(winkelgebied_code = V4_nw, item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) |>
    mutate(winkelgebied_naam = haven::as_factor(winkelgebied_code)),
  
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by= "name") |>
    group_by(winkelgebied_code = V20,  item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) |>
    mutate(winkelgebied_naam = haven::as_factor(winkelgebied_code))
) |>
  mutate(winkelgebied_code = str_pad(winkelgebied_code, width= 3, side = "left", pad= 0))


gem_rap |>
  pivot_wider(names_from = item, values_from = gemiddelde, values_fill = 0) |>
  filter (winkelgebied_naam != 'geen winkelstraat/gebied',
          winkelgebied_naam != 'overig',
          aantal > 19) |>
  write.xlsx( "04 output tabellen/tabel_rapportcijfers24.xlsx", withFilter=T, overwrite=T)


  
  
