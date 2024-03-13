

library(tidyverse)
library(openxlsx)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

load(file = "03 tussentijds/tabel_kkb_samenvatting.Rdata")

my_plot <- function (x, afzet_var){
  
  grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
  grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))
  
  font <- "Amsterdam Sans"
  
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

  blauw_pal<- c("#004699", "#3858a4", "#566bb0", "#707ebb", "#8992c6", "#a1a7d2", "#b8bcdd", "#d0d2e8", "#e7e8f4")
  
  hcl <- farver::decode_colour(blauw_pal, "rgb", "hcl")
  
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  
  
  x |>
  ggplot(aes(
    
    y = fct_rev(monitor),
    group = fct_rev({{afzet_var}}),
    x = value,
    label = value))+
    
    geom_col(
      aes(fill = {{afzet_var}}))+
    
    geom_text(
      aes(
        label = if_else(value > 10,as.character(round(value)),""),
        color = {{afzet_var}}),
      
      position = position_stack(vjust =0.5),
      family = font, 
      lineheight = .8)+
    
    labs(
      title=NULL, x=NULL, y = NULL) +
    
    theme_os2()+
    
    scale_fill_manual(
      name= NULL, values = blauw_pal[c(2,3,5,6,7,8)])+
    
    scale_color_manual(
      name= NULL, values = label_col[c(2,3,5,6,7,8)])+
    
    guides(
      fill = guide_legend(
        nrow =1, reverse = F), 
      colour = "none") 
  }


# fig dagelijks niet dagelijks amsterdam
tabel_samengevat_l |>
  filter(
    gbd_sdl_naam == 'totaal Amsterdam', 
    name %in% c("dagelijkse producten", "niet-dagelijkse producten totaal")) |>
  
  my_plot(afzet_stadsdeel_naam) +
  facet_wrap( ~ name)
ggsave("04 output tabellen/fig1_kkb_20_24.png", width = 7, height = 3)




tabel_centrum_l |>
  
  mutate(
    name = str_replace(
      name, "niet-dagelijkse producten totaal", "NDG totaal")) |>
  
  filter(
    name != "dagelijkse producten",
    name != "niet-dagelijkse producten recreatief", 
    name != "niet-dagelijkse producten doelgericht") |>

  my_plot(afzet_stadsdeel_naam) +
  facet_wrap( ~ fct_relevel(name, "NDG totaal", after = Inf))
ggsave("04 output tabellen/fig2_kkb_20_24.png", width = 7, height = 4)


# fig niet-dagelijks per stadsdeel 
tabel_samengevat_l |> 
    filter(name == "niet-dagelijkse producten totaal") |>
    my_plot(afzet_eigen_sd) +
  facet_wrap( ~ gbd_sdl_naam)
ggsave("04 output tabellen/fig3_kkb_20_24.png", width = 7, height = 4)


fig_4 <- tabel_samengevat_l |> 
  
  filter(name %in% c("dagelijkse producten", "niet-dagelijkse producten recreatief", "niet-dagelijkse producten doelgericht"),
         monitor == 'monitor 2024') |>
  
  ggplot(aes(
    y = fct_rev(gbd_sdl_naam),
    fill = fct_rev(afzet_eigen_sd) ,
    x = value))+
  geom_col()+
  
  geom_text(aes(label = if_else(value > 10,as.character(round(value)),"")), 
            position = position_stack(vjust =0.5),
            family=font, lineheight=.8)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os2()+ 
  scale_fill_manual(name= NULL, values = palettes_list$wild[c(2,3,5,6,7)])  +
  guides(fill = guide_legend(nrow =1, reverse = T)) +
  facet_wrap( ~ name)
ggsave("04 output tabellen/fig4_kkb_24_kso.png", width = 7, height = 4)


############################################
### toevloeiing vanuit andere stadsdelen ---
############################################


freq_kkb_24 <- read_rds ("03 tussentijds/freq_kkb_24.rds")

# naar stadsdeel
tabel_toevloeiing_sd <- freq_kkb_24$SD_toevloeiing_rel |>
  filter(
    afzet_stadsdeel_code != 'overig NL', 
    afzet_stadsdeel_code != "online", 
    afzet_stadsdeel_code != "MRA") 


# naar gebied
tabel_toevloeiing_geb <- freq_kkb_24$GEB_toevloeiing_rel |>
  filter(
    afzet_gebied_code != 'overig NL', 
    afzet_gebied_code != "online", 
    afzet_gebied_code != "MRA") 

library(sf)

pad_sd  <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/amsterdam/2022/stadsdelen-2022-geo.json"
pad_geb <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/amsterdam/2022/gebieden-2022-geo.json"


stadsdelen_sf <- read_sf(pad_sd) 
gebieden_sf   <- read_sf(pad_geb) 

tabel_toevloeiing_sd_sf<- tabel_toevloeiing_sd |>
  left_join(stadsdelen_sf, by = c("gbd_sdl_naam"= "naam"))|>
  st_as_sf() |>
  mutate(value_cut=os_cut(aantal_gw_omz_ink, c(0, 5, 10, 15, 20, 25 , 30, 50, 70, 100),    suffix = '%')) 

tabel_toevloeiing_geb_sf<- tabel_toevloeiing_geb |>
  left_join(gebieden_sf, by = c("gbd_ggw_naam"= "naam"))|>
  st_as_sf() |>
  mutate(value_cut=os_cut(aantal_gw_omz_ink, c(0, 5, 10, 15, 20, 25 , 30, 50, 70, 100),    suffix = '%')) 

# Centrum, Zuid en Zuidoost
tabel_toevloeiing_sd_sf |>
  filter(pdg_naam == 'modeartikelen',
         afzet_stadsdeel_code == "A") |>
  ggplot()+
  geom_sf(aes(fill = value_cut))+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os2()+ 
  scale_fill_manual(name= NULL, values = rev(palettes_list$blauw))  +
  guides(fill = guide_legend(nrow =1, reverse = T)) +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), panel.spacing = unit(0, "lines"), plot.background = element_blank()
  )
ggsave("04 output tabellen/fig5_kkb_24_toevl_mode_centrum.png", width = 7, height = 4)










