
# lees libraries in met setup
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

load(file = "03 tussentijds/data_24_DEF.Rdata")

data_def$data_20_def[["weeg_ams"]]<- data_def$data_20_def[["wstad"]]
data_def$data_22_def[["weeg_ams"]]<- data_def$data_22_def[["wg"]]


achtergrondvar<- c(
  "respdef",
  "opleiding_klas", "inkomen_klas", 
  "huishouden_klas", "geslacht", 
  "leeftijd_klas", 
  "gbd_brt_code", "gbd_brt_naam",
  "gbd_wijk_code","gbd_wijk_naam",
  "gbd_ggw_code", "gbd_ggw_naam",
  "gbd_sdl_code", "gbd_sdl_naam")

jaren<- c("monitor 2024", "monitor 2022", "monitor 2020")

data_v19 <-  data_def|>
  map (\(x) janitor::clean_names(x))|>
  map (\(x) select(x, all_of(achtergrondvar), v19, starts_with("v24"), weeg_ams))|>
  map2(jaren, \(x,y) add_column(x, monitor= y)) |>
  map (\(x) mutate(x , v19_naam = haven::as_factor(v19)))|>
  map (\(x) mutate(x , v19_naam = case_when(
    is.na(v19_naam) ~ 'weet ik niet, geen antwoord',
    v19_naam =='weet niet, geen antwoord' ~ 'weet ik niet, geen antwoord',
    TRUE ~ v19_naam)))

my_summary<- function (x, groupvars=NULL) {
  
  x |>
    group_by(monitor, v19_naam, {{groupvars}}) |>
    
    summarise(
      aantal     = n(),
      aantal_gew = sum(weeg_ams, na.rm = T)
    ) |>
    
    group_by(monitor, {{groupvars}}) |>
    
    mutate(
      aandeel     = round(aantal/sum(aantal, na.rm = T)*100,2),
      aandeel_gew = round(aantal_gew/ sum(aantal_gew, na.rm = T)*100,2)
    )
  
}

v19_levels <- c(
  "ja, elke dag",
  "ja, enkele keren per week" ,
  "ja, een tot drie keer per maand",
  "ja, enkele keren per jaar",
  "nee, nooit",
  "weet ik niet, geen antwoord")

sd_levels<- c(
  "Centrum",   "Westpoort",   "West",   "Nieuw-West", 
  "Zuid", "Oost", "Noord", "Weesp", "Zuidoost", 
  "Amsterdam totaal", "Amsterdam")

ink_levels <- c("inkomen laag", "inkomen midden", "inkomen hoog", "inkomen onbekend", "Amsterdam")

## Amsterdam totaal ---
tabel_v19_ams  <- data_v19 |>
  map_df (\(x) my_summary(x)) |>
  mutate(v19_naam     = factor(v19_naam,levels = v19_levels))







# fig dagelijks niet dagelijks amsterdam
tabel_v19_ams|>
  
  ggplot(aes(
    y = fct_rev(monitor),
    fill = fct_rev(v19_naam),
    x = aandeel_gew)
  )+
  
  geom_col()+
  geom_text(aes(label = if_else(aandeel_gew > 10,as.character(round(aandeel_gew)),"")), 
            position = position_stack(vjust =0.5),
            family=font, lineheight=.8)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os3()+ 
  scale_fill_manual(name= NULL, values = palettes_list$blauw[c(9, 6, 5,4,3,2)])  +
  guides(fill = guide_legend(reverse = T)) 
ggsave("04 output tabellen/fig10_wink_plezier.png", width = 7, height = 3)

# winkelen voor het plezier naar achtergrondgroep













### redenen winkelbezoek ---


# maak de labels 
redenen24_labels<-data_v19$data_24_def |>
  select(v2401:v2416 )|> 
  names()|>
  map_df(\(i) tibble(v24_labels = attr(data_v19$data_24_def[[i]], "label"), name = i) 
  ) 



redenen24<-data_v19$data_24_def |>
  select(all_of(achtergrondvar), v2401:v2416)|>
  pivot_longer(cols = c(v2401:v2416))|>
  left_join(redenen24_labels, by = "name")  |>
  mutate(v24_labels = case_when(
  v24_labels ==
  'ik vind het winkelgebied/winkelstraat waar ik vaak kom prettig winkelen (als in: overzichtelijk, functioneel, volledig, veilig)' ~ 
    'ik vind het winkelgebied waar ik kom prettig winkelen',
  TRUE ~ v24_labels))

my_reden_function<- function (x, achtergrondvar, achtergrondlev) {
  
  tabel <- bind_rows(
    
    # totaal
     x |>
      filter(!is.na(value))|>
      group_by(name, v24_labels, value)|>
      summarise(aantal_genoemd= n())|>
      group_by(name, v24_labels)|>
      mutate(aandeel=aantal_genoemd/ sum(aantal_genoemd)*100)|>
      filter(value == 1) |>
      add_column({{achtergrondvar}} := 'Amsterdam'),
    
    # achtergrondvar
    x |>
      filter(!is.na(value))|>
      group_by({{achtergrondvar}}, name, v24_labels, value)|>
      summarise(aantal_genoemd= n())|>
      group_by({{achtergrondvar}}, name, v24_labels)|>
      mutate(aandeel=aantal_genoemd/ sum(aantal_genoemd)*100)|>
      filter(value == 1)
  )
  
  figuur <- tabel |>
    mutate({{achtergrondvar}} := factor({{achtergrondvar}}, levels = achtergrondlev))|> 
  
    filter(
      aandeel > 15,
      v24_labels != 'weet niet',
      v24_labels != 'anders, namelijk',
      !is.na({{achtergrondvar}}))|>
    
    ggplot(aes(
      y = fct_reorder(v24_labels, aandeel), x = aandeel))+
    geom_col(fill= palettes_list$wild[3])+
    geom_text(aes(label = if_else(aandeel > 10,as.character(round(aandeel)),"")), 
              position = position_stack(vjust =0.5),
              family=font, lineheight=.8)+
    
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os2()+ 
    scale_fill_manual(name= NULL, values = palettes_list$wild[c(9,5,4,3)])  +
    guides(fill = guide_legend(nrow =1, reverse = T)) +
    facet_wrap(vars({{achtergrondvar}}), nrow=1)
  
  return(figuur)
  
  
}


opl_levels  <- c("praktisch opgeleid", "middelbaar opgeleid", "theoretisch opgeleid","Amsterdam")
gesl_levels <- c("vrouw", "man", "Amsterdam")
leefkl_levels <- c("35 jaar of jonger", "35 jaar tot en met 55 jaar", "55 jaar of ouder", "Amsterdam")



  
redenen24 |>
  filter(opleiding_klas!= 'opleiding onbekend')|>
  my_reden_function(opleiding_klas, opl_levels)+
  theme(axis.text.x = ggplot2::element_text (family = font, size = 10, angle = 0, vjust = 1, hjust=1))

redenen24 |>
  filter(geslacht!= 'onbekend')|>
  my_reden_function(geslacht, gesl_levels)+
  theme(axis.text.x = ggplot2::element_text (family = font, size = 10, angle = 0, vjust = 1, hjust=1))
ggsave("04 output tabellen/fig12_wink_plezier.png", width = 7, height = 3)



redenen24 |>
  filter(leeftijd_klas!= 'onbekend')|>
  my_reden_function(leeftijd_klas, leefkl_levels)+
  theme(axis.text.x = ggplot2::element_text (family = font, size = 10, angle = 0, vjust = 1, hjust=1))
ggsave("04 output tabellen/fig11_wink_plezier.png", width = 7, height = 3)




# "opleiding_klas", 
# "inkomen_klas", 
# "huishouden_klas",
# "geslacht", 
# "leeftijd_klas






