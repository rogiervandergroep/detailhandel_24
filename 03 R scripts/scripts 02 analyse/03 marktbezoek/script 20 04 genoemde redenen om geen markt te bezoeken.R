
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




opl_levels    <- c("praktisch opgeleid", "middelbaar opgeleid", "theoretisch opgeleid","Amsterdam")
gesl_levels   <- c("vrouw", "man", "Amsterdam")
leefkl_levels <- c("35 jaar of jonger", "35 jaar tot en met 55 jaar", "55 jaar of ouder", "leeftijd onbekend", "Amsterdam")
ink_levels    <- c("inkomen laag", "inkomen midden", "inkomen hoog", "onbekend", "Amsterdam")
sd_levels     <- c("Centrum","Westpoort","West" , "Nieuw-West","Zuid" ,"Oost" , "Noord","Weesp",  "Zuidoost" ,  "Amsterdam", NA )




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


grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))

font <- "Amsterdam Sans"

blauw_pal <- c("#004699", "#3858a4", "#566bb0", "#707ebb", "#8992c6", "#a1a7d2", "#b8bcdd", "#d0d2e8", "#e7e8f4")

hcl <- farver::decode_colour(blauw_pal, "rgb", "hcl")

label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

theme_os2 <- function(orientation="vertical", legend_position = "bottom"){
  
  
  
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family = font, size = 12),
      axis.text = ggplot2::element_text(family = font, size = 12),
      plot.caption = ggplot2::element_text(family = font, size = 12),
      axis.title = ggplot2::element_text(family = font, hjust = 1, size = 12),
      plot.subtitle = ggplot2::element_text(family = font, size = 12),
      legend.text = ggplot2::element_text(family = font, size = 12),
      plot.title = ggplot2::element_text(family = font, lineheight = 1.2, size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.title=element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position=legend_position,
      panel.border = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.text = ggplot2::element_text(color = "black", family = font, face = "bold", size = 12)
    ) 
  
  if (orientation %in% c("vertical", "v")){
    theme <- theme + ggplot2::theme(panel.grid.major.x = element_blank())
  } else if (orientation %in% c("horizontal", "h")){
    theme <- theme + ggplot2::theme(panel.grid.major.y = element_blank())
  }
  
}






my_figure <- function (x, achtergrondvar) {
  
  x |>
    filter(
      value %in% c(
        "ga uit gewoonte nooit naar de markt", 
        "geen tijd", "te duur",
        "producten van te lage kwaliteit",  
        "te weinig variatie in producten", 
        "te ongezellig, te weinig sfeer")
    )|>
    
    mutate(value = case_when(
      value == 'combinatie van winkels in deze buurt en een oninteressante markt' ~ 'genoeg winkels en oninteressante markt in buurt',
      TRUE ~ value)) |>
  
    ggplot(aes(
      y = fct_relevel(fct_reorder(value, aandeel_gew), "anders"), 
      x = aandeel_gew))+
    
    geom_col(fill = blauw_pal[2])+
    geom_text(aes(label = if_else(aandeel_gew > 5,as.character(round(aandeel_gew)),"")), 
              position = position_stack(vjust =0.5),
              family=font, lineheight=.8, color= "white")+
    
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os2()+ 
    scale_fill_manual(name= NULL, values = blauw_pal[2])  +
    guides(fill = guide_legend(nrow =1, reverse = T)) +
    facet_wrap(vars({{achtergrondvar}}))
  
  
}

# redenen geen martkbezoek per stadsdeel
test<- red_geenmarkt_v14 |> 
  my_geen_reden_function(gbd_sdl_naam, sd_levels)|>
  my_figure(gbd_sdl_naam)
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








