source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# waardering buurt voor dagelijkse boodschappen

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

data_v8 <-  data_def|>
  map (\(x) select(x, all_of(achtergrondvar), starts_with("v8"), weeg_ams))|>
  map2(jaren, \(x,y) add_column(x, monitor= y)) |>
  map (\(x) janitor::clean_names(x))|>
  map (\(x) mutate(x, v8_naam = haven::as_factor(v8)))|>
  map (\(x) mutate(x , v8_naam= case_when(
    is.na(v8_naam) ~ 'weet ik niet, geen antwoord',
    v8_naam =='weet niet, geen antwoord' ~ 'weet ik niet, geen antwoord',
    TRUE ~ v8_naam)))

my_summary<- function (x, groupvars=NULL) {
  
  x |>
    group_by(monitor, v8_naam, {{groupvars}}) |>
    
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
    

v8_levels <- c("ja", "enigszins", "nee", "weet ik niet, geen antwoord")

sd_levels<- c("Centrum", "Westpoort", "West", "Nieuw-West", "Zuid", 
              "Oost", "Noord", "Weesp", "Zuidoost", "Amsterdam totaal", "Amsterdam")

tabel_v8  <- bind_rows(
  
  data_v8 |>
  map_df (\(x) my_summary(x)) |>
  add_column(gbd_sdl_code = 'Amsterdam totaal',
             gbd_sdl_naam = 'Amsterdam totaal'),
  

  data_v8 |>
  map_df (\(x) my_summary(x, groupvars = across(all_of(c("gbd_sdl_code", "gbd_sdl_naam")))))|>
    filter(!is.na(gbd_sdl_naam))
  ) |>
  
  mutate(
    v8_naam      = factor(v8_naam,levels = v8_levels),
    gbd_sdl_naam = factor(gbd_sdl_naam, levels = sd_levels)
    )|>
  rename (categorie = v8_naam) |>
  add_column(vraag = 'v8 oordeel aanbod winkels dagelijkse boodschappen in de buurt')
  
  
tab_mondet24 <- list()

tab_mondet24$v8 <- tabel_v8 |>
  select(monitor, vraag, categorie, gbd_sdl_naam,  aandeel_gew)


# fig dagelijks niet dagelijks amsterdam

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

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")



tabel_v8 |>
  
  # verwijderen data Weesp uit vorige monitors
  mutate(aandeel_gew=case_when(
    gbd_sdl_naam == 'Weesp' & monitor %in% c("monitor 2020", "monitor 2022") ~ NA,
    TRUE ~ aandeel_gew)) |>
  
  filter(gbd_sdl_naam != 'Westpoort') |>
  
  ggplot(aes(
    y = fct_rev(gbd_sdl_naam),
    fill = fct_rev(v8_naam),
    x = aandeel_gew)
    )+
  
  geom_col()+
  geom_text(aes(label = if_else(aandeel_gew > 10,as.character(round(aandeel_gew)),"")), 
            position = position_stack(vjust =0.5),
            family=font, lineheight=.8)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os2()+ 
  scale_fill_manual(name= NULL,  values = blauw_pal[c(9,5,4,3)])  +
  scale_color_manual(name= NULL, values = label_col[c(9,5,4,3)]) +
  guides(fill = guide_legend(nrow =1, reverse = T)) +
  facet_wrap( ~ monitor)
ggsave("04 output tabellen/fig1_dg_buurt.png", width = 7, height = 3)


### tevredenheid per gebied in 2024

# aandeel dat tevreden is - 
tabel_v8_geb  <- data_v8$data_24_def |>
  my_summary(groupvars = across(all_of(c("gbd_ggw_code", "gbd_ggw_naam"))))|>
    filter(!is.na(gbd_ggw_naam),
           v8_naam == 'ja') |>
  mutate(aandeel_cut= os_cut(
    aandeel_gew,
    breaks = c(0, 30, 40, 50, 60, 70, 80, 100), suffix = "%"))
  
library(sf)
kaart_gebieden <- os_get_geom("gebieden")
  

  # samenvoegen tabel aan wijkenkaart -
kaart_gebieden2 <- kaart_gebieden |>
    left_join(tabel_v8_geb, by = c("code"= "gbd_ggw_code")) 
  
  #plot de kaart
kaart_gebieden2 |>
  filter(aantal > 10) |>
    ggplot(aes(fill=aandeel_cut))+
    geom_sf(data = kaart_gebieden,aes(fill= NULL), color = "white", size = 2.2)+
    geom_sf(color = "white", size = 2.2)+
    scale_fill_manual(name= NULL, values = palettes_list$stoplicht[c(1,3,4,5,7)], drop=T)  +
    theme_ois_map(legend_position = "bottom")+
    guides(fill = guide_legend( title = NULL, ncol= 2))
  ggsave("04 output tabellen/fig2_marktbez_kaart_geb.png", width= 5, height = 4)
  
# redenen van ontevredenheid
  
# maak de labels 
redenen24_labels<-data_v8$data_24_def |>
  select(v8b_nw01:v8b_nw11)|> 
    names()|>
    map_df(\(i) tibble(v8_labels = attr(data_v8$data_24_def[[i]], "label"), name = i) 
    ) |>
  mutate(v8_labels=case_when(
    v8_labels == 'te weinig aanbod van streekproducten/lokaal geproduceerde producten' ~ 'weinig aanbod streek-, lokaal prod.',
    v8_labels == 'aanbod producten in winkels in deze buurt heeft lage kwaliteit' ~ 'aanbod heeft lage kwaliteit',
    v8_labels == 'te weinig aanbod van seizoensgebonden producten' ~ 'weinig aanbod seizoensgebonden prod.',
    v8_labels == 'winkels in de buurt zijn ongezellig, hebben weinig sfeer' ~ 'winkels in buurt zijn ongezellig',
    v8_labels == 'te weinig winkels met biologische producten' ~ 'weinig winkels met biol. prod.',
    v8_labels == 'ik mis bepaalde producten, namelijk' ~ 'ik mis bepaalde producten',
    TRUE ~ v8_labels)
  )


  
redenen24<-data_v8$data_24_def |>
  select(all_of(achtergrondvar), v8b_nw01:v8b_nw11)|>
  pivot_longer(cols = c(v8b_nw01:v8b_nw11))|>
  left_join(redenen24_labels, by = "name")

tabel_reden<- bind_rows(
  
  redenen24 |>
    filter(!is.na(value))|>
    group_by(name, v8_labels, value)|>
    summarise(aantal_genoemd= n())|>
    group_by(name, v8_labels)|>
    mutate(aandeel=aantal_genoemd/ sum(aantal_genoemd)*100)|>
    filter(value == 1) |>
    add_column(gbd_sdl_naam= 'Amsterdam'),
  
  redenen24 |>
    filter(!is.na(value))|>
    group_by(gbd_sdl_naam, name, v8_labels, value)|>
    summarise(aantal_genoemd= n())|>
    group_by(gbd_sdl_naam, name, v8_labels)|>
    mutate(aandeel=aantal_genoemd/ sum(aantal_genoemd)*100)|>
    filter(value == 1)
) |>
  mutate(gbd_sdl_naam=factor(gbd_sdl_naam, levels = sd_levels))


tabel_reden |>
  filter(v8_labels != 'weet niet',
         v8_labels != 'anders, namelijk',
         !is.na(gbd_sdl_naam))|>
  
  ggplot(aes(
  y = fct_reorder(v8_labels, aandeel),x = aandeel))+
  geom_col(fill= palettes_list$wild[3])+
  geom_text(aes(label = if_else(aandeel > 10,as.character(round(aandeel)),"")), 
            position = position_stack(vjust =0.5),
            family=font, lineheight=.8)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os2()+ 
  scale_fill_manual(name= NULL, values = palettes_list$wild[c(9,5,4,3)])  +
  guides(fill = guide_legend(nrow =1, reverse = T)) +
  facet_wrap( ~ gbd_sdl_naam, nrow = 3)
ggsave("04 output tabellen/fig3_reden_geenmarktbez.png", width= 8, height = 6)


