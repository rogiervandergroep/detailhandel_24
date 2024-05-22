

# lees libraries in met setup
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")


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

  
df_rap_dg0 <-  data_def$data_24_def |>
  select(any_of(c("Respondent_ID",  "weeg_ams_ink", "V4_nw", rap_v7))) |>
  add_column(productgroep="winkelgebied voor dagelijkse boodschappen") |>
  filter(!is.na(V4_nw))

df_rap_ndg0 <- data_def$data_24_def |>
  select(any_of(c("Respondent_ID",  "weeg_ams_ink", "V20", rap_v22)))|>
  add_column(productgroep="winkelgebied voor niet-dagelijkse boodschappen")|>
  filter(!is.na(V20))

# toevoegen stadsdeel en gebied van winkelgebied

wg_sd<- read.xlsx("02 lookup tabellen/winkelgebieden24.xlsx")

#chistiaan huygensplein en weesp aanpassen
my_weesp<- function (x){
  
  x |>
    mutate(
      winkelgebied_code = case_when(
        winkelgebied_code == '105' ~ '106',
        winkelgebied_code == '208' ~ '312',
        TRUE ~ winkelgebied_code)
      ) |>
    
    mutate(
      winkelgebied_naam = case_when(
        winkelgebied_naam == "Christiaan Huygensplein, Watergraafsmeer, Oost" ~ "Christiaan Huygensplein, Helmholzstraat, Watergraafsmeer, Oost",
        winkelgebied_naam == "Weesp" ~ 'Weesp, Centrum (oude binnenstad, Achtergracht, Nieuwstad, Oude Gracht, Nieuwstraat)',
        TRUE ~ winkelgebied_naam)
      )
  
  
}



df_rap_dg <- df_rap_dg0 |>
  mutate(winkelgebied_code=str_pad(V4_nw, width = 3 , side = "left", pad = "0"))|>
  left_join(wg_sd, by = "winkelgebied_code")|>
  my_weesp()


df_rap_ndg <- df_rap_ndg0 |>
  mutate(winkelgebied_code=str_pad(V20, width = 3 , side = "left", pad = "0"))|>
  left_join(wg_sd, by = "winkelgebied_code") |>
  my_weesp()



# toevoegen labels rapportcijfer items
labels_dg <- df_rap_dg |>
  names()|>
  map_df(\(i) tibble(labels = attr(df_rap_dg[[i]], "label"), name = i))|>
  mutate(labels = str_replace_all(labels, ":.*", ""),
         labels = str_replace_all(labels, "[']", ""),
         labels = case_when(
           name == "V4_nw" ~ "In welk winkelgebied doet u normaliter de boodschappen?",
           name == "V6" ~ "Wat is uw totaaloordeel over dit winkelgebied?",
           TRUE ~ labels))


labels_ndg <- df_rap_ndg |>
  names()|>
  map_df(\(i) tibble(labels = attr(df_rap_ndg[[i]], "label"), name = i))|>
  mutate(labels = str_replace_all(labels, ":.*", ""),
         labels = str_replace_all(labels, "[']", ""),
         labels = case_when(
           name == "V20" ~ "Wat is uw favoriete winkelgebied in Amsterdam of de regio om te winkelen voor uw plezier?",
           name == "V21" ~ "Wat is uw totaaloordeel over dit winkelgebied?",
           TRUE ~ labels))

###################################
# rapportcijfers per winkelgebied -
###################################

gem_rap  <- bind_rows(
  
  # rapportcijfer per item per winkelgebied DG
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7) ) |>
    left_join(labels_dg, by= "name") |>
    group_by(winkelgebied_code, winkelgebied_naam, afzet_stadsdeel_code, item = labels, productgroep) |>
    summarise(
      aantal     = n(),
      gemiddelde = round(mean(value,   na.rm = T), 2)),
  
  # gemiddelde rapportcijfer per winkelgebied DG
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7) ) |>
    left_join(labels_dg, by= "name") |>
    group_by(winkelgebied_code, winkelgebied_naam, afzet_stadsdeel_code,  item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?')|>
    group_by(winkelgebied_code, winkelgebied_naam, afzet_stadsdeel_code, productgroep)|>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2))|>
    add_column(item = 'gemiddeld rapportcijfer'),
  
  # rapportcijfer per item per winkelgebied NDG
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by= "name") |>
    group_by(winkelgebied_code, winkelgebied_naam, afzet_stadsdeel_code,  item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) ,
  
  # rapportcijfer per winkelgebied NDG
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by= "name") |>
    group_by(winkelgebied_code, winkelgebied_naam, afzet_stadsdeel_code, item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2))  |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?')|>
    group_by(winkelgebied_naam, winkelgebied_code, afzet_stadsdeel_code, productgroep)|>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2))|>
    add_column(item = 'gemiddeld rapportcijfer')
  ) 

############################
# rapportcijfers Amsterdam -
############################

gem_rap_ams  <- bind_rows(
  
  # rapportcijfers per item amsterdam DG
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7) ) |>
    left_join(labels_dg, by= "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) ,
  
  # gemiddeld raportcijfer amsterdam DG
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7) ) |>
    left_join(labels_dg, by= "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2))|>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?')|>
    group_by(productgroep)|>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2))|>
    add_column(item = 'gemiddeld rapportcijfer'),
  
  # rapportcijfers per item amsterdam NDG
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by= "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)),
  
  # gemiddeld raportcijfer amsterdam NDG
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by= "name") |>
    group_by(item = labels, productgroep) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?')|>
    group_by(productgroep)|>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2))|>
    add_column(item = 'gemiddeld rapportcijfer')
  ) |>
  add_column(
    winkelgebied_code = 'ams',
    winkelgebied_naam = 'Amsterdam totaal')



############################
# rapportcijfers stadsdeel -
############################

gem_rap_sd  <- bind_rows(
  
  # rapportcijfers per item sd DG
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7) ) |>
    left_join(labels_dg, by= "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) ,
  
  # gemiddeld raportcijfer sd DG
  df_rap_dg |>
    pivot_longer(cols = any_of(rap_v7) ) |>
    left_join(labels_dg, by= "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2))|>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?')|>
    group_by(productgroep, afzet_stadsdeel_code)|>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2))|>
    add_column(item = 'gemiddeld rapportcijfer'),
  
  # rapportcijfers per item sd NDG
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by= "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)),
  
  # gemiddeld raportcijfer sd NDG
  df_rap_ndg |>
    pivot_longer(cols = any_of(rap_v22)) |>
    left_join(labels_ndg, by= "name") |>
    group_by(item = labels, productgroep, afzet_stadsdeel_code) |>
    summarise(aantal     = n(),
              gemiddelde = round(mean(value, na.rm = T), 2)) |>
    filter(item != 'Wat is uw totaaloordeel over dit winkelgebied?')|>
    group_by(productgroep, afzet_stadsdeel_code)|>
    summarise(
      aantal = mean(aantal),
      gemiddelde = round(mean(gemiddelde, na.rm = T), 2))|>
    add_column(item = 'gemiddeld rapportcijfer')
  
  ) |>
  
  mutate(winkelgebied_naam = case_when(
    afzet_stadsdeel_code == "A" ~ "Centrum",
    afzet_stadsdeel_code == "E" ~ "West",
    afzet_stadsdeel_code == "F" ~ "Nieuw-West",
    afzet_stadsdeel_code == "K" ~ "Zuid",
    afzet_stadsdeel_code == "M" ~ "Oost",
    afzet_stadsdeel_code == "N" ~ "Noord",
    afzet_stadsdeel_code == "S" ~ "Weesp",
    afzet_stadsdeel_code == "T" ~ "Zuidoost"))

###############
# categorieën - 
###############

totaal<- c(
  "totaaloordeel winkelgebied")

gemiddelde<- c(
  "gemiddeld rapportcijfer")

sfeer <- c(
  "uiterlijk van de winkels",                  
  "aankleding en inrichting",                         
  "sfeer en de gezelligheid") 

aanbod <- c(
  "aanbod van daghoreca",
  "het algemeen prijsniveau",                         
  "het diverse aanbod van food-winkels" ,             
  "het diverse aanbod van non-food-winkels") 

duurzaam_bio <- c(
  "het biologische/duurzame aanbod van food-winkels", 
  "het duurzame aanbod van non-food winkels") 

overlast <- c(
  "het schoonhouden van de straten",
  "overlast door horeca",    
  "overlast van andere mensen",                       
  "veiligheid winkelomgeving overdag",                
  "veiligheid winkelomgeving ‘s avonds",
  "overlast door vervuiling") 

bereik <- c(
  "parkeermogelijkheden voor auto",                   
  "parkeermogelijkheden voor fiets", 
  "algemene bereikbaarheid")  





# samenvoegen -
gem_totaal <- bind_rows(gem_rap,gem_rap_ams, gem_rap_sd ) |>
  
  mutate(
    item = str_trim(item, "both")
    ) |>
  
  mutate(item = case_when(
    item == "Wat is uw totaaloordeel over dit winkelgebied?" ~ "totaaloordeel winkelgebied",
    item == "het biologische/duurzame aanbod van food-winkels" ~ "het biologische/duurzame aanbod food",
    item == "het schoonhouden van de straten/ stoepen/ passage" ~ "schoonhouden van de straten",
    item == "de sfeer en de gezelligheid van het winkelgebied" ~ "sfeer en de gezelligheid",
    item == "de aankleding en inrichting van het winkelgebied (faciliteiten, verlichting, bankjes)" ~ "aankleding en inrichting",
    item == "het uiterlijk van de winkels (denk aan gevels, etalages, inrichting)" ~ "uiterlijk van de winkels",
    TRUE ~ item)
    )|>
  
  mutate(thema=case_when(
    item %in% sfeer ~ 'sfeer en gezelligheid',
    item %in% totaal ~ 'totaal oordeel',
    item == "gemiddeld rapportcijfer" ~ 'gemiddelde rapportcijfer',
    item %in% duurzaam_bio ~ 'duurz. en biol. producten',
    item %in% aanbod ~ 'aanbod producten',
    item %in% overlast ~ 'overlast',
    item %in% bereik ~ 'bereik')
    ) |>
  
  filter (
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig')|>
  
  mutate(
    winkelgebied_naam_kort=str_split(winkelgebied_naam, ",", simplify = TRUE)[, 1])|>
  
  mutate(winkelgebied_naam_kort = case_when(
    winkelgebied_naam_kort == 'Waddenweg / Meeuwenlaan / Gedempt Hamerkanaal'  ~ 'Meeuwenlaan e.o.',
    winkelgebied_naam_kort == 'Bezaanjachtplein / Winkelcentrum in de Banne' ~ 'Bezaanjachtplein / In de Banne',
    winkelgebied_naam_kort == 'Zeilstraat / Hoofddorpplein/ Sloterkade' ~ 'Zeilstraat / Hoofddorpplein',
    winkelgebied_naam_kort == 'Ferdinand Bolstraat / Marie Heinekenplein' ~ 'F. Bolstraat / M. Heinekenplein',
    TRUE ~ winkelgebied_naam_kort))

gem_thema <- gem_totaal |>
  group_by(winkelgebied_code, winkelgebied_naam, afzet_stadsdeel_code, productgroep,
           aantal,thema, winkelgebied_naam_kort) |>
  summarise(gemiddelde=mean(gemiddelde, na.rm = T))|>
  filter(!is.na(gemiddelde)) 





  

############### 
### KAARTEN ---
############### 

# inlezen theme en ggplot
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

wg_sd_kl <- wg_sd |>
  select(winkelgebied_code, winkelgebied_oisnaam, winkelgebied_oiscode)|>
  filter(!is.na(winkelgebied_code))


tabel_figuur<-gem_totaal|>  
  filter(aantal > 19) |>
  
  pivot_wider(names_from = item, values_from = gemiddelde, values_fill = 0, id_cols = -thema) |>
  filter (winkelgebied_naam != 'geen winkelstraat/gebied',
          winkelgebied_naam != 'overig') |>
  left_join(wg_sd_kl, by= "winkelgebied_code") |>
  relocate (winkelgebied_code, winkelgebied_naam,winkelgebied_oiscode, winkelgebied_oisnaam)

write.xlsx(tabel_figuur, "04 output tabellen/tab_mondet24_rapportcijfers24.xlsx", withFilter=T, overwrite=T)

tabel_figuur<-gem_totaal|>  
  filter(aantal > 19) |>
  
  filter (winkelgebied_naam != 'geen winkelstraat/gebied',
          winkelgebied_naam != 'overig') |>
  left_join(wg_sd_kl, by= "winkelgebied_code") |>
  relocate (winkelgebied_code, winkelgebied_naam,winkelgebied_oiscode, winkelgebied_oisnaam)

write.xlsx(tabel_figuur, "04 output tabellen/tabel_rapportcijfers24_DG_NDG.xlsx", withFilter=T, overwrite=T)



tabel_figuur_lng<-gem_totaal|>  
  filter(aantal > 19,
         productgroep == 'winkelgebied voor dagelijkse boodschappen') |>
  
  filter (winkelgebied_naam != 'geen winkelstraat/gebied',
          winkelgebied_naam != 'overig') |>
  left_join(wg_sd_kl, by= "winkelgebied_code") |>
  relocate (winkelgebied_code, winkelgebied_naam,winkelgebied_oiscode, winkelgebied_oisnaam)

write.xlsx(tabel_figuur_lng, "04 output tabellen/tabel_rapportcijfers24_lng.xlsx", withFilter=T, overwrite=T)




# figuur met rapportcijfers
gem_totaal|>
  filter(
    aantal > 49,
    productgroep == 'winkelgebied voor dagelijkse boodschappen',
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig',
    winkelgebied_naam_kort != 'Noord overig',
    !is.na(winkelgebied_code)) |>

  ggplot(aes(x = fct_relevel(fct_rev(fct_reorder(item, gemiddelde)), c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"), after = Inf), 
             y = fct_relevel(fct_reorder(winkelgebied_naam_kort, gemiddelde), "Amsterdam totaal"),
             fill = gemiddelde)) +
  geom_tile(color = "white",lwd = 0.9,linetype = 1) +
  labs(title=NULL, x=NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  theme_os2()+
  coord_fixed(0.6)
ggsave("04 output tabellen/fig4_rap_20_24.png", width = 7, height = 8)


# figuur met samenvatting thema's


gem_totaal|>
  filter(
    aantal > 49,
    productgroep == 'winkelgebied voor dagelijkse boodschappen',
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig',
    winkelgebied_naam_kort != 'Noord overig',
    !is.na(winkelgebied_code)) |>
  
  ggplot(aes(y = fct_relevel(fct_reorder(thema, gemiddelde), c("totaal oordeel", "gemiddelde rapportcijfer"), after = Inf), 
             x = fct_rev(fct_relevel(fct_reorder(winkelgebied_naam_kort, gemiddelde), "Amsterdam totaal")),
             fill = gemiddelde)) +
  geom_tile(color = "white",lwd = 1.1,linetype = 1) +
  labs(title=NULL, x=NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  theme_os2()+
  coord_fixed(0.6)
ggsave("04 output tabellen/fig8_rap_thema_24.png", width = 7, height = 4)


### tabel per stadsdeel ---

gem_totaal|>
  filter(productgroep == 'winkelgebied voor dagelijkse boodschappen',
         is.na(winkelgebied_code),
         afzet_stadsdeel_code != 'MRA',
         afzet_stadsdeel_code != 'overig NL')|>

  
  ggplot(aes(y = fct_relevel(fct_reorder(item, gemiddelde), c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer")), 
             x = fct_rev(fct_reorder(winkelgebied_naam_kort, gemiddelde)),
             fill = gemiddelde)) +
  geom_tile(color = "white",lwd = 1.1,linetype = 1) +
  labs(title=NULL, x=NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  theme_os2()+
  coord_fixed(0.6)
ggsave("04 output tabellen/fig5_rap_20_24.png", width = 7, height = 5)



gem_totaal|>
  filter(
    aantal > 19,
    productgroep != 'winkelgebied voor dagelijkse boodschappen',
    winkelgebied_naam != 'geen winkelstraat/gebied',
    winkelgebied_naam != 'overig',
    winkelgebied_naam_kort != 'Noord overig',
    winkelgebied_naam_kort != 'Centrum overig') |>
  
  ggplot(aes(x = fct_relevel(fct_rev(fct_reorder(item, gemiddelde)), c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"), after = Inf), 
             y = fct_relevel(fct_reorder(winkelgebied_naam_kort, gemiddelde), "Amsterdam totaal"),
             fill = gemiddelde)) +
  geom_tile(color = "white",lwd = 1.1,linetype = 1) +
  labs(title=NULL, x=NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  theme_os2()+
  coord_fixed(0.6)


cor_matrix|>
  pivot_longer(if_numeric)
  ggplot(aes(x = fct_relevel(fct_rev(fct_reorder(item, gemiddelde)), c("totaaloordeel winkelgebied", "gemiddeld rapportcijfer"), after = Inf), 
             y = fct_relevel(fct_reorder(winkelgebied_naam_kort, gemiddelde), "Amsterdam totaal"),
             fill = gemiddelde)) +
  geom_tile(color = "white",lwd = 1.1,linetype = 1) +
  labs(title=NULL, x=NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  theme_os2()+
  coord_fixed(0.6)


ggsave("04 output tabellen/fig6_rap_20_24.png", width = 7, height = 5)


