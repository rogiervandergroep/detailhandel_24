
### gedrag, gewoonten duurzaam consumeren --- 

### lees libraries in met setup ---
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

### opstarten ---
load("03 tussentijds/data_24_DEF.Rdata")

# data_def$data_20_def[["weeg_ams"]]<- data_def$data_20_def[["wstad"]]
# data_def$data_22_def[["weeg_ams"]]<- data_def$data_22_def[["wg"]]

achtergrondvar<- c(
  "respdef",
  "opleiding_klas", "inkomen_klas", 
  "huishouden_klas", "geslacht", 
  "leeftijd_klas", 
  "gbd_brt_code", "gbd_brt_naam",
  "gbd_wijk_code","gbd_wijk_naam",
  "gbd_ggw_code", "gbd_ggw_naam",
  "gbd_sdl_code", "gbd_sdl_naam")



# v1b1: v1b5, v1b_anders
# v1c01: v1c10, v1c_anders
# v18b1:v18b5
# v18c...

data_gedrag<- list()

data_gedrag$v1b<- data_def$data_24_def |>
  janitor::clean_names() |>
  as_factor() |>
  select(all_of(starts_with("v1b")),all_of(achtergrondvar), weeg_ams)

data_gedrag$v1c<- data_def$data_24_def |>
  janitor::clean_names() |>
  as_factor() |>
  select( all_of(starts_with("v1c")),all_of(achtergrondvar), weeg_ams)

data_gedrag$v18b<- data_def$data_24_def |>
  janitor::clean_names() |>
  as_factor() |>
  select(all_of(starts_with("v18b")),all_of(achtergrondvar), weeg_ams)

data_gedrag$v18c<- data_def$data_24_def |>
  janitor::clean_names() |>
  as_factor() |>
  select(all_of(starts_with("v18c")),all_of(achtergrondvar), weeg_ams)

# leefregels 
labels_gedrag <- list()
         
# maak de labels 
labels_gedrag$v1c <-data_gedrag$v1c |>
  select(v1c01:v1c10 )|> 
  names()|>
  map_df(\(i) tibble(v1c_labels = attr(data_gedrag$v1c[[i]], "label"), name = i) 
  ) 


# tabel leefregels 
data_gedrag$v1c<-data_gedrag$v1c  |>
  select(all_of(achtergrondvar), v1c01:v1c10 )|>
  pivot_longer(cols = c(v1c01:v1c10 ))|>
  left_join(labels_gedrag$v1c, by = "name")  





my_leefstijl_function<- function (x, achtergrondvar) {
  
  bind_rows(
    
    # totaal
    x |>
      filter(!is.na(value))|>
      group_by(name, v1c_labels, value)|>
      summarise(aantal_genoemd= n())|>
      group_by(name, v1c_labels)|>
      mutate(aandeel=aantal_genoemd/ sum(aantal_genoemd)*100)|>
      filter(value == 'Yes') |>
      add_column({{achtergrondvar}} := 'Amsterdam'),
    
    # achtergrondvar
    x |>
      filter(!is.na(value))|>
      group_by({{achtergrondvar}}, name, v1c_labels, value)|>
      summarise(aantal_genoemd= n())|>
      group_by({{achtergrondvar}}, name, v1c_labels)|>
      mutate(aandeel=aantal_genoemd/ sum(aantal_genoemd)*100)|>
      filter(value == 'Yes')
  )
  
}
  
test<- data_gedrag$v1c |>
  filter(opleiding_klas != 'opleiding onbekend')|>
  my_leefstijl_function(opleiding_klas)






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
  
  



opl_levels  <- c("praktisch opgeleid", "middelbaar opgeleid", "theoretisch opgeleid","Amsterdam")
gesl_levels <- c("vrouw", "man", "Amsterdam")
leefkl_levels <- c("35 jaar of jonger", "35 jaar tot en met 55 jaar", "55 jaar of ouder", "Amsterdam")




redenen24 |>
  filter(opleiding_klas!= 'opleiding onbekend')|>
  my_reden_function(opleiding_klas, opl_levels)

redenen24 |>
  filter(geslacht!= 'onbekend')|>
  my_reden_function(geslacht, gesl_levels)
ggsave("04 output tabellen/fig12_wink_plezier.png", width = 7, height = 3)



redenen24 |>
  filter(leeftijd_klas!= 'onbekend')|>
  my_reden_function(leeftijd_klas, leefkl_levels)
ggsave("04 output tabellen/fig11_wink_plezier.png", width = 7, height = 3)




# "opleiding_klas", 
# "inkomen_klas", 
# "huishouden_klas",
# "geslacht", 
# "leeftijd_klas






