
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# data gemaakt in 'script 20 01 bezoek markten.R' inlezen
data_markt_def <- read_rds("03 tussentijds/data_markt_def.RDS") 




achtergrondvar<- c(
  "respdef",
  "opleiding_klas", "inkomen_klas", 
  "huishouden_klas", "geslacht", 
  "leeftijd_klas", 
  "gbd_brt_code", "gbd_brt_naam",
  "gbd_wijk_code","gbd_wijk_naam",
  "gbd_ggw_code", "gbd_ggw_naam",
  "gbd_sdl_code", "gbd_sdl_naam")


### aandeel marktbezoek ---

red_geenmarkt_v14 <- data_markt_def$data_24|> 
select(all_of(achtergrondvar), v14_nw1, v14_nw2, v14_nw3, weeg_ams)|>
  pivot_longer(cols = c(v14_nw1, v14_nw2, v14_nw3) ) 


my_geen_reden_function<- function (x, achtergrondvar, achtergrondlev) {
  
  tabel <- bind_rows(
    
    # totaal
    x |>
      filter(!is.na(value))|>
      group_by(value)|>
      summarise(aantal_genoemd= n(),
                aantal_gew = sum(weeg_ams, na.rm = T))|>
      ungroup()|>
      mutate(aandeel     = aantal_genoemd/ sum(aantal_genoemd)*100,
             aandeel_gew = aantal_gew    / sum(aantal_gew)*100)|>
      add_column({{achtergrondvar}} := 'Amsterdam'),
    
    # achtergrondvar
    x |>
      filter(!is.na(value),
             !is.na({{achtergrondvar}}))|>
      group_by({{achtergrondvar}}, value)|>
      summarise(aantal_genoemd= n(),
                aantal_gew = sum(weeg_ams, na.rm = T))|>
      group_by({{achtergrondvar}})|>
      mutate(aandeel     = aantal_genoemd/ sum(aantal_genoemd)*100,
             aandeel_gew = aantal_gew    / sum(aantal_gew)*100)
  ) |>
    mutate({{achtergrondvar}} := factor({{achtergrondvar}}, levels = achtergrondlev)) 


}

my_figure <- function (x, achtergrondvar) {
  
  x |>
    filter(
      aandeel_gew > 4,
      value != "weet niet",
      value != "bang voor besmetting") |>
    
    mutate(value = case_when(
      value == 'combinatie van winkels in deze buurt en een oninteressante markt' ~ 'genoeg winkels en oninteressante markt in buurt',
      TRUE ~value)) |>
  
    ggplot(aes(
      y = fct_relevel(fct_reorder(value, aandeel_gew), "anders"), x = aandeel_gew))+
    geom_col(fill= palettes_list$wild[3])+
    geom_text(aes(label = if_else(aandeel_gew > 6,as.character(round(aandeel_gew)),"")), 
              position = position_stack(vjust =0.5),
              family=font, lineheight=.8)+
    
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os3()+ 
    scale_fill_manual(name= NULL, values = palettes_list$wild[c(9,5,4,3)])  +
    guides(fill = guide_legend(nrow =1, reverse = T)) +
    facet_wrap(vars({{achtergrondvar}}))
  
  
}





opl_levels    <- c("praktisch opgeleid", "middelbaar opgeleid", "theoretisch opgeleid","Amsterdam")
gesl_levels   <- c("vrouw", "man", "Amsterdam")
leefkl_levels <- c("35 jaar of jonger", "35 jaar tot en met 55 jaar", "55 jaar of ouder", "leeftijd onbekend", "Amsterdam")
ink_levels    <- c("inkomen laag", "inkomen midden", "inkomen hoog", "onbekend", "Amsterdam")
sd_levels     <- c("Centrum","Westpoort","West" , "Nieuw-West","Zuid" ,"Oost" , "Noord","Weesp",  "Zuidoost" ,  "Amsterdam", NA )



geen_marktbezoek_sd <- red_geenmarkt_v14 |> 
  my_geen_reden_function(gbd_sdl_naam, sd_levels)|>
  my_figure(gbd_sdl_naam)

geen_marktbezoek_sd
ggsave("04 output tabellen/fig30_reden_geenmarkt_sd.png", width = 9, height = 6)



geen_marktbezoek_leefkl<- red_geenmarkt_v14 |> 
  filter(leeftijd_klas != 'onbekend') |>
  my_geen_reden_function(leeftijd_klas, leefkl_levels) |>
  my_figure(leeftijd_klas)
ggsave("04 output tabellen/fig30_reden_geenmarkt_lft.png", width = 7, height = 6)







redenen24 |>
  filter(leeftijd_klas!= 'onbekend')|>
  my_reden_function(leeftijd_klas, leefkl_levels)
ggsave("04 output tabellen/fig11_wink_plezier.png", width = 7, height = 3)




# "opleiding_klas", 
# "inkomen_klas", 
# "huishouden_klas",
# "geslacht", 
# "leeftijd_klas








