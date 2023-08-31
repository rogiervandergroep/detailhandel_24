
library(tidyverse)
library(openxlsx)

### inlezen werkbetand 

data22<- readRDS("02 lookup tabellen/werkbestand consumenten enquete.rds")





# inlezen lookup tabel winkelgebieden
df_wg  <- read.xlsx("02 lookup tabellen/22015 schakeltabel wg_enq naar wg_ois.xlsx")

df_wg$code_enq<- str_pad(df_wg$code_enq, pad = "0", side="left", width = 3)

  
kolommen_kkb<- c("Respondent_ID", "V1_nw","V1_nw_Codes",
                 "V18_nw_modeartikelen_GV1","V18_nw_modeartikelen_GV1_Codes", 
                 "V18_nw_elektronica_GV1","V18_nw_elektronica_GV1_Codes",
                 "V18_nw_huishoudelijk_GV1","V18_nw_huishoudelijk_GV1_Codes",
                 "V18_nw_woninginrichting_GV1","V18_nw_woninginrichting_GV1_Codes",
                 "V18_nw_doehetzelf_GV1","V18_nw_doehetzelf_GV1_Codes",
                 "V18_nw_planten_GV1","V18_nw_planten_GV1_Codes",
                 "V18_nw_media_GV1","V18_nw_media_GV1_Codes",
                 "V18_nw_sportspel_GV1","V18_nw_sportspel_GV1_Codes",
                 "gbd_wijk_code","gbd_wijk_naam",
                 "gbd_ggw_code","gbd_ggw_naam",
                 "gbd_sdl_code","gbd_sdl_naam", "weegfactor" ,
                 "opleiding", "inkomen", "leeftijdklas")

data22_kkb<- data22|>
  select(any_of(kolommen_kkb))


## codes v1_nw_codes: dagelijks

# 2 = online
# 3 en 4 = onbekend

## codes v18_nw...: niet dagelijks

# 2 en 3 = online
# 4 en 5 = onbekend

my_online_dg <- function (.x,  var1, var2){
  
   .x |>
    
    mutate (across(c({{var1}}, {{var2}}), as.character))  |>
    
    mutate("{{var1}}" :=
             case_when(
               {{var2}}  == "2"               ~  "800",
               ({{var2}} == "3" | {{var2}} == "4") ~  "999",
               TRUE                           ~ {{var1}}
               )
           )

  
}

my_online_ndg <- function (.x,  var1, var2){
  
  .x |>
    
    mutate (across(c({{var1}}, {{var2}}), as.character))  |>
    
    mutate("{{var1}}" :=
             case_when(
               ({{var2}} == "4"| {{var2}} == "5") ~  "999",
               ({{var2}} == "2"| {{var2}} == "3") ~  "800",
               TRUE                           ~ {{var1}}
             )
    )
  
  
}


data22_kkb<- data22_kkb |>
  my_online_dg(V1_nw, V1_nw_Codes)|>
  my_online_ndg(V18_nw_modeartikelen_GV1, V18_nw_modeartikelen_GV1_Codes)|>
  my_online_ndg(V18_nw_elektronica_GV1, V18_nw_elektronica_GV1_Codes)|>
  my_online_ndg(V18_nw_huishoudelijk_GV1, V18_nw_huishoudelijk_GV1_Codes)|>
  my_online_ndg(V18_nw_woninginrichting_GV1, V18_nw_woninginrichting_GV1_Codes)|>
  my_online_ndg(V18_nw_planten_GV1, V18_nw_planten_GV1_Codes)|>  
  my_online_ndg(V18_nw_doehetzelf_GV1, V18_nw_doehetzelf_GV1_Codes)|>
  my_online_ndg(V18_nw_media_GV1, V18_nw_media_GV1_Codes)|>
  my_online_ndg(V18_nw_sportspel_GV1, V18_nw_sportspel_GV1_Codes)

# voeg weegfactor per wijk toe


# # inlezen respons
# df_inw <- read.xlsx("02 lookup tabellen/22015 basis voor steekproeftrekking 2022 2023 steek 2.xlsx")|>
#   mutate(inw_aandeel = inwoners22/sum(inwoners22),
#          resp_aandeel= respons_t1/sum(respons_t1),
#          weegfactor  = case_when(resp_aandeel == 0 ~ 1,
#                                  TRUE ~ (inw_aandeel/resp_aandeel)))
# 
# data22_kkb<-data22_kkb |>
#   left_join(df_inw[, c("wijk_code", "weegfactor")],
#             by=c("gbd_wijk_code"="wijk_code"))


#totaal
koopkrachtbinding<- data22_kkb |>
  select(-ends_with("_Codes"))|>
  pivot_longer(cols= matches("_nw"), names_to = "productgroep", values_to = "winkelgebied")|>
  filter(!is.na(winkelgebied),
       winkelgebied != 999,
       winkelgebied != 309) |>
  group_by(productgroep, winkelgebied) |>
  summarise(aantal_gew= n()) 
  #summarise(aantal_gew= sum(weegfactor, na.rm=T))

#inkomen
koopkrachtbinding_ink<- data22_kkb |>
  select(-ends_with("_Codes"))|>
  pivot_longer(cols= matches("_nw"), names_to = "productgroep", values_to = "winkelgebied")|>
  filter(!is.na(winkelgebied),
         winkelgebied != 999,
         winkelgebied != 309) |>
  group_by(productgroep, winkelgebied, inkomen) |>
  summarise(aantal_gew= n())

#leeftijd
koopkrachtbinding_lft<- data22_kkb |>
  select(-ends_with("_Codes"))|>
  pivot_longer(cols= matches("_nw"), names_to = "productgroep", values_to = "winkelgebied")|>
  filter(!is.na(winkelgebied),
         winkelgebied != 999,
         winkelgebied != 309) |>
  group_by(productgroep, winkelgebied, leeftijdklas) |>
  summarise(aantal_gew= n())
  

koopkracht_list<- list(koopkrachtbinding, koopkrachtbinding_ink, koopkrachtbinding_lft)
names(koopkracht_list)<- c("koopkrachtbinding", "koopkrachtbinding_ink", "koopkrachtbinding_lft")

my_mutate<- function(x){
  
  x |>
    mutate(winkelgebied=str_pad(winkelgebied,pad = "0", side="left",width = 3))|>
    left_join(df_wg , by = c("winkelgebied"= "code_enq"))|>
    mutate(codewg_ois=str_pad(codewg_ois,pad = "0", side="left",width = 3))
  
  }


koopkracht_list <- map(koopkracht_list, my_mutate)

# inlezen lookup tabel mt omzetcijfers (bron: https://www.retailinsiders.nl/data/)
df_omzet  <- read.xlsx("02 lookup tabellen/22015 schakeltabel wg_enq naar wg_ois.xlsx", sheet = "omzetcijfers")


my_groupby <- function(x, achtergrondvar){
  
  x |>
  
  group_by(productgroep, aandeel_amsterdam, {{achtergrondvar}}) |>
    summarise(aantal  = sum(aantal_gew, na.rm=T)) |>
    group_by(productgroep, {{achtergrondvar}}) |>
    mutate(aandeel = aantal/sum(aantal, na.rm=T)*100) |>
    
    left_join(df_omzet, by=c("productgroep")) |>
    mutate(aantal_wg=aantal*weegfactor)
  
  
}
my_totaal_ndg<- function (x, achtergrondvar){
  
  x |>
  
  filter(productgroep != 'V1_nw') |>
    group_by(aandeel_amsterdam, {{achtergrondvar}})|>
    summarise(aantal= sum(aantal),
              aantal_wg=sum(aantal_wg))|>
    group_by({{achtergrondvar}})|>
    mutate(aandeel_ong=aantal/sum(aantal)*100,
           aandeel_gew=aantal_wg/sum(aantal_wg)*100)|>
    add_column(productgroep='totaal niet-dagelijkse producten')
  
  
  
  
}


## gewogen ---
tabel_kkb_wg<- koopkracht_list$koopkrachtbinding|>
  my_groupby()

tabel_kkb_ink_wg<- koopkracht_list$koopkrachtbinding_ink|>
  my_groupby(inkomen)

tabel_kkb_lft_wg<- koopkracht_list$koopkrachtbinding_lft|>
  my_groupby(leeftijdklas)

tabel_kkb_def<- bind_rows(tabel_kkb_wg, tabel_kkb_ink_wg, tabel_kkb_lft_wg)



### totaal amsterdam: niet dagelijkse boodschappen  ---

tabel_kkb_ams <- tabel_kkb_wg |>
  my_totaal_ndg()

tabel_kkb_ink_ams <- tabel_kkb_ink_wg |>
  my_totaal_ndg(inkomen)

tabel_kkb_lft_ams <- tabel_kkb_lft_wg |>
  my_totaal_ndg(leeftijdklas)

tabel_kkb_ndg_def<- bind_rows(tabel_kkb_ams,tabel_kkb_ink_ams,  tabel_kkb_lft_ams)
rm(tabel_kkb_ams,tabel_kkb_ink_ams,  tabel_kkb_lft_ams)


# definitieve tabel ---

tabel_productgroep <-tabel_kkb_def |>
  select(productgroep, aandeel_amsterdam, aandeel, leeftijdklas, inkomen)

tabel_totaal_ndg <- tabel_kkb_ndg_def |>
  select(productgroep, aandeel_amsterdam, aandeel_gew, leeftijdklas, inkomen)|>
  rename(aandeel=aandeel_gew)

tabel22_23<- bind_rows(tabel_productgroep, tabel_totaal_ndg) |>
  pivot_wider(names_from = productgroep, values_from = aandeel)|>
  add_column(jaar="monitor 2022 2023")


names(tabel22_23) <- c("gebied", "leeftijd", "inkomen",
                  "dagelijkse_boodschappen",
                  "doehetzelfproducten",  
                  "elektronica", 
                  "huishoudelijk", 
                  "media",     
                  "modeartikelen",   
                  "bloemen_planten",          
                  "sport_spel", 
                  "woninginrichting" ,  
                  "totaal_niet_dagelijks", "jaar")


tabel22_23<- tabel22_23|>
  pivot_longer(where(is.numeric)) |>
  mutate(gebied=factor(gebied, levels= c("Centrum", "overig Amsterdam", "MRA", "overig Nederland", "online")))


# definititieve tabel inkomen - 


###  wegschrijven definitieve tabel

write.xlsx(tabel22_23, "02 lookup tabellen/tabel_koopkrachtbinding23.xlsx", overwrite = T)

### grafiek ---

# data tot 2022
tabel19_21<- read.xlsx("02 lookup tabellen/tabel_koopkrachtbinding22.xlsx", sheet= 'totaal')|>
  pivot_longer(where(is.numeric))

tabel_def<- bind_rows(tabel22_23, tabel19_21)

tabel_def_wide<- tabel_def |>
  pivot_wider(names_from = name, values_from = value)|>
  write.xlsx("tabel2022.xlsx", overwrite = T)


tabel_def <-  tabel_def |>
  mutate(
    inkomen =factor(inkomen, levels = c("inkomen laag","inkomen midden","inkomen hoog","weet niet, geen antwoord")),
    gebied=as.character(gebied),
    online_ams=case_when(gebied %in% c('Centrum', 'overig Amsterdam') ~ 'Amsterdam',
                              gebied == 'online' ~ 'online',
                              TRUE ~ gebied),
    gebied  =factor(gebied, levels =  c("Centrum","overig Amsterdam","overig Nederland","MRA", "online")))

  
  

                                                     
                                                      
source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


library(ggplot2)

figuur<- tabel_def |>
  
  filter(
    is.na(inkomen),
    is.na(leeftijd),
    gebied %in% c("Centrum", "overig Amsterdam", "online"),
    name   %in% c("dagelijkse_boodschappen","totaal_niet_dagelijks"))|>
  
  mutate(name=case_when(
    name == 'dagelijkse_boodschappen' ~ 'dagelijkse boodschappen',
    name == 'totaal_niet_dagelijks'   ~ 'totaal niet-dagelijkse producten',
    name == 'huishoudelijk'           ~ 'huishoudelijke producten',
    name == 'modeartikelen'           ~ 'kleding en mode',
    TRUE ~name),
         
         jaar=case_when(jaar == 'monitor 2019 2020' ~ '2019',
                        jaar == 'monitor 2020 2021' ~ '2021',
                        jaar == 'monitor 2022 2023' ~ '2023')) |>
  ggplot(aes(x=jaar,
             y=value,
             fill=online_ams))+
  geom_col(position = "dodge" )+
  labs(y="koopkrachtbinding (%)", x="veldwerkperiode")+
  scale_fill_manual(NULL,
                    values = wild_pal)+
  theme_os()+
  facet_wrap(~name)
           
figuur
ggsave("04 output tabellen/koopkrachtbinding19_23.png", width=8, height = 4)
ggsave("04 output tabellen/koopkrachtbinding19_23.svg")



figuur_ink<- tabel_def |>
  
  filter(
    jaar=='monitor 2022 2023',
    !is.na(inkomen),
    inkomen != 'weet niet, geen antwoord',
    is.na(leeftijd),
    gebied %in% c("Centrum", "overig Amsterdam", "online"),
    name   %in% c("totaal_niet_dagelijks"))|>
  
  mutate(name=case_when(
    name == 'dagelijkse_boodschappen' ~ 'dagelijkse boodschappen',
    name == 'totaal_niet_dagelijks' ~ 'totaal niet-dagelijkse producten',
    name == 'huishoudelijk'         ~ 'huishoudelijke producten',
    name == 'modeartikelen'         ~ 'kleding en mode',
    TRUE ~name)) |>
  ggplot(aes(x=inkomen,y=value,fill=gebied))+
  geom_col(position = "dodge")+
  labs(y="koopkrachtbinding (%)", x=NULL)+
  scale_fill_manual(NULL,values = wild_pal)+
  theme_os()
ggsave("04 output tabellen/koopkrachtbinding23_ink.png", width=8, height = 4)
ggsave("04 output tabellen/koopkrachtbinding23_ink.svg")


figuur_lft<- tabel_def |>
  
  filter(
    jaar=='monitor 2022 2023',
    !is.na(leeftijd),
    leeftijd != 'onbekend',
    is.na(inkomen),
    gebied %in% c("Centrum", "overig Amsterdam", "online"),
    name   %in% c("totaal_niet_dagelijks"))|>
  
  mutate(name=case_when(
    name == 'dagelijkse_boodschappen' ~ 'dagelijkse boodschappen',
    name == 'totaal_niet_dagelijks' ~ 'totaal niet-dagelijkse producten',
    name == 'huishoudelijk'         ~ 'huishoudelijke producten',
    name == 'modeartikelen'         ~ 'kleding en mode',
    TRUE ~name)) |>
  ggplot(aes(x=leeftijd,y=value,fill=gebied))+
  geom_col(position = "dodge")+
  labs(y="koopkrachtbinding (%)", x=NULL)+
  scale_fill_manual(NULL,values = wild_pal)+
  theme_os()
ggsave("04 output tabellen/koopkrachtbinding23_lft.png", width=8, height = 4)
ggsave("04 output tabellen/koopkrachtbinding23_lft.svg")
