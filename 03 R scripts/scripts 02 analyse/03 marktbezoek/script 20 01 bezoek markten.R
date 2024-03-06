
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

library(janitor)

### opstarten ---

load("03 tussentijds/data_24_DEF.Rdata")

# v2: hoeveel uitgegeven aan boodschappen

# V3_1: supermarkt
# V3_2: andere winkels
# V3_3: markt
# V3_4: online
# V3_5: anders

# v13: Hoe vaak bezoekt u een markt?
# v14: Wat zijn de drie belangrijkste redenen om de markt niet te bezoeken
# v15: Op welke markt koopt u het meest?
# v15_anders: andere markt namelijk ...
# v16: hoeveel besteedt u op de markt?



achtergrondvar<- c(
  "respdef",
  "opleiding_klas", "inkomen_klas", 
  "huishouden_klas", "geslacht", 
  "leeftijd_klas", 
  "gbd_brt_code", "gbd_brt_naam",
  "gbd_wijk_code","gbd_wijk_naam",
  "gbd_ggw_code", "gbd_ggw_naam",
  "gbd_sdl_code", "gbd_sdl_naam")

data_markt <- list()

# selectie van data met v13 tm v15 warenmarkten
data_markt$data_24 <- data_def$data_24_def |>
  clean_names()|>
  as_factor()|>
  select(v2, contains("v3"),
         v13:v15_anders, v16, 
         all_of(achtergrondvar), weeg_ams)|>
  
  select(-c(
            "v3_rij1_num_codes",  
            "v3_rij2_num_codes",  
            "v3_rij3_num_codes",  
            "v3_rij4_num_codes",  
            "v3_rij5_num_codes",  
            "v3_esc")) 

data_markt$data_22 <- data_def$data_22_def |>
  clean_names()|>
  as_factor()  |>
  select(v2, contains("v3"),
         v13:v15_anders, v16, 
         all_of(achtergrondvar), wg)|>
  
  select(-c(
            "v3_rij1_num_codes",  
            "v3_rij2_num_codes",  
            "v3_rij3_num_codes",  
            "v3_rij4_num_codes",  
            "v3_rij5_num_codes",  
            "v3_esc"))   |>
  
  mutate(v13= case_when(is.na(v13) ~ "zelden tot nooit",
                        TRUE ~v13))


data_markt$data_20 <- data_def$data_20_def |>
  clean_names()|>
  as_factor()  |>
  select(v2, contains("v3"),
         v13:v15_anders, v16, 
         all_of(achtergrondvar), wstad) |>
  mutate(v13= case_when(v13 =='1 keer week' ~ "1 keer per week",
                        TRUE ~v13))

# inlezen opgeschoonde namen v15
markt_uniek <- read.xlsx("03 tussentijds/markt_uniek.xlsx")



my_markt_function <- function(x){
  
  x |>
  
  left_join(markt_uniek, by="v15") |>
    
    # wel versus geen marktbezoek 
    mutate(
      v15_schoon = case_when(
        v13 %in% c('weet niet, geen antwoord','zelden tot nooit') ~ 'bezoekt geen markt',
        is.na(v15) & ( v13!='weet niet, geen antwoord' | v13!= 'zelden tot nooit') ~'bezoekt markt, markt onbekend',
        TRUE ~ v15_schoon)) |>
    
    mutate(
      marktbezoek = case_when(
        v15_schoon == 'bezoekt geen markt' ~ 'bezoekt geen markt',
        TRUE ~ 'bezoekt wel een markt')) |>
    
    # opdelen stadsdelen in drie gebieden -
    mutate(gebieden=case_when(
      gbd_sdl_naam == 'Centrum'~ 'Centrum',
      gbd_sdl_naam %in% c('Zuid', 'West', 'Oost') ~ 'Zuid, West Oost', 
      TRUE ~ 'NW, Noord, ZO, Weesp'))
  
}

data_markt_def <- list()

data_markt_def <- data_markt |>
  map(\(x) my_markt_function(x))
  
write_rds(data_markt_def, file= "03 tussentijds/data_markt_def.RDS")

jaren<- c("monitor 2024", "monitor 2022", "monitor 2020")

kosten <- data_markt_def |>
  map (\(x)   group_by (x, v15_schoon))|>
  map (\(x)   summarise(x,aantal = n(),uitgaven= mean(v16, na.rm=T)))|>
  map2(jaren, \(x,y) add_column(x, monitor= y))|>
  bind_rows() |>
  filter(
    aantal>10,
    v15_schoon != 'bezoekt geen markt',
    v15_schoon != 'MRA',
    v15_schoon != 'anders',
    v15_schoon != 'bezoekt markt, markt onbekend')


kosten|>
  
  filter(monitor == 'monitor 2024')|>
  
  ggplot(aes(
    y = fct_reorder(v15_schoon, uitgaven),
    x = uitgaven))+
  
  geom_col(fill=palettes_list$wild[3])+
  geom_text(aes(label = glue::glue("€ {as.character(round(uitgaven))},-")), 
            position = position_stack(vjust =0.5),
            family=font, lineheight=.8)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os3()+ 
  guides(fill = guide_legend(reverse = T)) 
ggsave("04 output tabellen/fig12_kosten_markt.png", width = 7, height = 5)
      

      
