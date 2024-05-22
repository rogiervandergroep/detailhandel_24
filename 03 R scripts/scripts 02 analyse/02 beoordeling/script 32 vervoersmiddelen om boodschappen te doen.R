
# V5 hoe gaat u normaliter boodschappen doen?


### lees libraries in met setup ---
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

### opstarten ---
load("03 tussentijds/data_24_DEF.Rdata")


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

data_v5<- data_def |>
  map(\(x) janitor::clean_names(x)) |>
  map(\(x) as_factor(x)) |>
  map(\(x) select(x, v5, v5_anders, all_of(achtergrondvar), weeg_ams)) |>
  map2(jaren, \(x,y) add_column(x, monitor= y)) |>
  bind_rows() |>
  filter(!is.na(v5)) |>
  mutate(
    v5=str_trim(v5, "both"),
    v5=case_when(
      v5 == 'bromfiets'~'brommer',
      v5 == 'brommer, bromfiets, scooter'~'brommer',
      v5 == 'anders, namelijk' ~ 'weet niet',
      v5 == 'anders' ~ 'weet niet',
      v5 == 'weet niet, geen antwoord' ~ 'weet niet',
      TRUE ~ v5))



sd_levels    <- c("Centrum","Westpoort","West","Nieuw-West","Zuid","Oost","Noord","Weesp","Zuidoost","Amsterdam totaal","Amsterdam")
ink_levels   <- c("inkomen laag","inkomen midden","inkomen hoog", "inkomen onbekend")  
huish_levels <- c("een persoon", "paar zonder kinderen","een ouder","paar met kinderen", "overig, of huishoudtype onbekend")
leeft_levels <- c('35 jaar of jonger', "35 jaar tot en met 55 jaar", '55 jaar of ouder', 'onbekend')
  
  
             

my_v5_function <- function(x, var, var_levels) {
  
  x |>
    group_by(monitor, onderwerp = {{var}}, v5) |>
    summarise(
      aantal     = n(),
      aantal_gew = sum(weeg_ams, na.rm=T))|>
    group_by(monitor, onderwerp) |>
    mutate(
      aandeel     = round(aantal / sum (aantal, na.rm=T) * 100, 2),
      aandeel_gew = round(aantal_gew / sum (aantal_gew, na.rm=T)*100, 2))
      }

tabel_v5_achtergr <- bind_rows(
  
  data_v5 |>
    group_by(v5, monitor) |>
    summarise(aantal     = n(),
              aantal_gew = sum(weeg_ams, na.rm=T))|>
    group_by(monitor) |>
    mutate(aandeel     = round(aantal / sum (aantal, na.rm=T) * 100, 2),
           aandeel_gew = round(aantal_gew / sum (aantal_gew, na.rm=T)*100, 2)) |>
    add_column(
      thema = "Amsterdam",
      onderwerp = 'Amsterdam'),
  
  
  data_v5 |>
    my_v5_function (gbd_sdl_naam)|>
    add_column(thema = "stadsdeel")|>
    mutate(onderwerp = factor(onderwerp, levels = sd_levels)),
  
  data_v5 |>
    my_v5_function (huishouden_klas)|>
    add_column(thema = "huishoudtype")|>
    mutate(onderwerp = factor(onderwerp, levels = huish_levels)),

  
  data_v5 |>
    my_v5_function(leeftijd_klas)|>
    add_column(thema = "leeftijdsklasse")|>
    mutate(onderwerp = factor(onderwerp, levels = leeft_levels)),

  data_v5 |>
    my_v5_function (inkomen_klas)|>
    add_column(thema= "inkomensklasse")|>
    mutate(onderwerp = factor(onderwerp, levels = ink_levels))

) |>
  rename (categorie = v5 ) |>
  add_column(vraag= 'v5 meest gekozen vervoersmiddel om boodschappen te doen')



tab_mondet24$v5 <- tabel_v5_achtergr |>
  select(monitor, vraag, categorie, onderwerp, thema,  aandeel_gew)


write.xlsx(tab_mondet24, "04 output tabellen/tab_mondet24_overigevragen.xlsx")



my_plot <- function (x, yvar){
  
  grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
  grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))
  
  font <- "Amsterdam Sans"
  
  blauw_pal<- c("#004699", "#3858a4", "#566bb0", "#707ebb", "#8992c6", "#a1a7d2", "#b8bcdd", "#d0d2e8", "#e7e8f4")
  
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
  
  
  x |>
    ggplot(aes(
      
      y     = fct_rev({{yvar}}),
      fill  = fct_relevel(fct_reorder(v5, aandeel_gew), "weet niet"),
      label = aandeel_gew,
      x     = aandeel_gew))+
    
    geom_col()+
    
    geom_text(
      aes(
        label = if_else(aandeel_gew >  10 ,as.character(round(aandeel_gew)),""),
        color = fct_relevel(fct_reorder(v5, aandeel_gew), "weet niet")),
      
      position = position_stack(vjust =0.5),
      family = font, 
      lineheight = .8)+
    
    labs(
      title=NULL, x=NULL, y = NULL) +
    
    theme_os2()+
    
    scale_fill_manual(
      name= NULL, values = blauw_pal[c(9,8,7,6,5,3,1)])+
    
    scale_color_manual(
      name= NULL, values = label_col[c(9,8,7,6,5,3,1)])+
    
    guides(
      fill = guide_legend(nrow = 3, reverse = T), 
      colour = "none") 
}

tabel_v5|>
  my_plot(monitor)
ggsave("04 output tabellen/figv5_mobiliteit.png", width = 8, height = 3)  


tabel_v5_achtergr|>
  
  filter(
    monitor == 'monitor 2024',
    onderwerp != 'inkomen onbekend',
    onderwerp != 'overig, of huishoudtype onbekend',
    onderwerp != 'onbekend',
    !is.na(onderwerp))|>
  
  my_plot(onderwerp)+
  facet_wrap(~ thema , scales= "free", ncol= 2)
ggsave("04 output tabellen/figv5_mobiliteit_sd.png", width = 8, height = 5)  

