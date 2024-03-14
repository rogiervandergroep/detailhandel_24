
### figuren ---


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

load("03 tussentijds/tabellen_markten_def.RData")



# frequentie ams
tabel_list_marktfreq$tab_v1_ams|>
  ggplot(aes(x = aandeel, y = fct_rev(monitor), fill = fct_rev(v13) ))+
  geom_col()+
  geom_text(
    aes(
      label = if_else(aandeel_gew > 5,as.character(round(aandeel_gew)),""),
      color = fct_rev(v13)), 
     position = position_stack(vjust =0.5),
     family=font, lineheight=.8)+
  
  labs(title = NULL, x = NULL, y = NULL) +
  theme_os2()+ 
  scale_fill_manual(name= NULL, values  = blauw_pal[c(9, 8, 7, 6,4,3,1)]) +
  scale_color_manual(name= NULL, values = label_col[c(9, 8, 7, 6,4,3,1)]) +
  guides(fill = guide_legend(reverse = T, ncol=2), color = 'none')
ggsave("04 output tabellen/fig1_freqmarktbez_ams.png", width= 7, height = 4)

# frequentie per sd
tabel_list_marktfreq$tab_v1_sd|>
  filter(
    monitor == 'monitor 2024',
    gbd_sdl_naam!= 'Westpoort',
    !is.na(gbd_sdl_naam))|>
  ggplot(aes(x= aandeel_gew, y= fct_rev(gbd_sdl_naam), fill= fct_rev(v13) ))+
  geom_col()+
  geom_text(
    aes(
      label = if_else(aandeel_gew > 5,as.character(round(aandeel_gew)),""),
      color = fct_rev(v13)), 
    position = position_stack(vjust =0.5),
    family=font, lineheight=.8)+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values  = blauw_pal[c(9, 8, 7, 6,4,3,1)]) +
  scale_color_manual(name= NULL, values = label_col[c(9, 8, 7, 6,4,3,1)]) +
  guides(fill = guide_legend(reverse = T, ncol=2), color = 'none')
ggsave("04 output tabellen/fig2_freqmarktbez_sd.png", width= 7, height = 4)


my_plot <- function (x, groupvar, fillvar = marktbezoek ){
  
  x |>
    
    ggplot(aes(x= aandeel_gew, y= {{groupvar}}, fill= {{fillvar}} ))+
    geom_col()+
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = wild_pal) +
    guides(fill = guide_legend(reverse = T))
  
  
}

# aandeel dat geen markt bezoekt per stadsdeel -
tabel_list_geenmarkt$tab_v1_sd|>
  filter(
    monitor =="monitor 2024",
    gbd_sdl_naam != 'Westpoort',
    !is.na(gbd_sdl_naam))|>
  
  my_plot(fct_rev(gbd_sdl_naam))
ggsave("04 output tabellen/fig1_geenmarktbez_sd.png", width= 7, height = 4)

# Samenvoegen Amsterdam totaal met Stadsdelen - 

tabel_AMS_SD<- bind_rows(
  
  tabel_list_geenmarkt$tab_v1_ams |>
    add_column(gbd_sdl_code='AMS',
               gbd_sdl_naam='Amsterdam'), 
  
  tabel_list_geenmarkt$tab_v1_sd)

# naar leeftijd
tabel_list_geenmarkt$tab_v1_leefklas|>

  
  filter(monitor == "monitor 2024",
         !is.na(leeftijd_klas),
         leeftijd_klas != 'anders',
         leeftijd_klas != 'onbekend')|>
  my_plot(fct_rev(leeftijd_klas) )
ggsave("04 output tabellen/fig1_geenmarktbez_lft.png", width= 7, height = 4)
ggsave("04 output tabellen/fig1_geenmarktbez_lft.svg", width= 7, height = 4)

# naar inkomen
tabel_list_geenmarkt$tab_v1_ink  |>
  
  filter(
    monitor == "monitor 2024",
    !is.na(inkomen_klas ),
    inkomen_klas != 'inkomen onbekend')|>
  
  ggplot(aes(x= aandeel, y= fct_rev(inkomen_klas), fill= marktbezoek ))+
  geom_col()+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = wild_pal) +
  guides(fill = guide_legend(nrow = 2, reverse = T))



max_markt <- tabel_list_marktnaam$tab_v1_ams|>
  filter(v15_schoon!= 'bezoekt geen markt',
         v15_schoon!= 'bezoekt markt, markt onbekend',
         monitor =='monitor 2024')|>
  slice_max(aandeel_gew, n=15, with_ties = F) |>
  pull( v15_schoon)

# meest bezochte markt totaal
tabel_list_marktnaam$tab_v1_ams|>
  filter(v15_schoon %in% max_markt)|>
  mutate(v15_schoon=factor(v15_schoon, levels = max_markt))|>
  select(-aantal)|>
  pivot_wider(values_from = aandeel_gew, names_from = monitor , values_fill = 0) |>
  pivot_longer(cols= c('monitor 2020': 'monitor 2024'), names_to = 'monitor', values_to = 'aandeel_gew') |>
  ggplot(aes(x= aandeel_gew, 
             y= fct_rev(v15_schoon),
             fill=fct_rev(monitor)))+
  geom_col(position = "dodge")+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os2()+ 
  scale_fill_manual(name= NULL, values = blauw_pal[c(2,5,8)]) +
  guides(fill = guide_legend(nrow = 1, reverse = T))
ggsave("04 output tabellen/fig2_marktbez_ams.png", width= 7, height = 5)


### meest bezochte markt per stadsdeel ---

tabel_list_marktnaam$tab_v1_sd |>
  
  filter(
    !is.na(gbd_sdl_naam),
    monitor == 'monitor 2024',
    v15_schoon != 'bezoekt markt, markt onbekend',
    gbd_sdl_naam != 'Westpoort',
    !is.na(v15_schoon))|>
  
  mutate(v15_schoon=fct_lump_n(v15_schoon, n= 6, w= aandeel, other_level = "overig")) |>
  
  ggplot(aes(x= aandeel,
             y= fct_relevel(fct_reorder(v15_schoon, aandeel), "bezoekt geen markt", "overig", "anders")))+
  
  geom_col(fill= wild_pal[2], position="dodge")+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = blauw_pal[c(2,5,8)]) +
  guides(fill = guide_legend(reverse = T, ncol=2))+
  facet_wrap(~gbd_sdl_naam, scales = 'free_y', ncol= 2)
ggsave("04 output tabellen/fig2_marktbez_sd.png", width= 6, height = 8)



my_plot_sd_ink  <- function (x) {
  
  # meest bezochte markt per stadseel naar inkomen
  x |>
    
    filter(!is.na(gbd_sdl_naam),
           v15_schoon != 'bezoekt markt, markt onbekend',
           !is.na(v15_schoon),
           inkomen_klas!= "inkomen onbekend",
           monitor == 'monitor 2024',
           !is.na(inkomen_klas))|>
    
    select(monitor: gbd_sdl_naam, aandeel_gew) |>
    
    pivot_wider(names_from = inkomen_klas, values_from = aandeel_gew, values_fill = 0)|>
    
    pivot_longer(cols=  c("inkomen laag","inkomen midden", "inkomen hoog", "totaal"), 
                 values_to = "aandeel_gew", 
                 names_to = "inkomen_klas") |>
    
    mutate(v15_schoon=fct_lump_n(v15_schoon, n= 5, w= aandeel_gew, other_level = "overig")) |>
    
    mutate(inkomen_klas=factor(
      inkomen_klas, 
      levels= c("inkomen laag", "inkomen midden", "inkomen hoog", "totaal")))|>
    

    ggplot(aes(x= aandeel_gew,
               fill= fct_rev(inkomen_klas),
               y= fct_relevel(fct_reorder(v15_schoon, aandeel_gew), "bezoekt geen markt", "overig")))+
    
    geom_col(position="dodge" )+
    labs(title = NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = blauw_pal[c(7,4,2,9)]) +
    guides(fill = guide_legend(nrow =2, reverse = T)) +
    theme()+
    facet_wrap(~ gbd_sdl_naam, scale= 'free_y', ncol=2)
  
  
}

my_plot_sd_ink(tabel_list_marktnaam$tab_v1_sd_ink)
ggsave("04 output tabellen/fig2_marktbez_sd.png", width= 7, height = 7)



  




markt_kleur<- c("#004699", "#f39f34", "#a4ccf3","#dceafa",
                "#53b361", "#bed200", "#ffe600","#ff9100",
                "#ec0000", "#fdb0cb", "#d48fb9","#71abdd",
                "#e50082", "#fb9bbe", "#efd2e3","#7c84b2",
                "#a2bad3", "#a00078", "#009de6","#fff3a7")

library(sf)
### plot kaart met meest genoemde markt per gebied -

# wijkenkaart van amsterdam - 
kaart_wijk <- read_sf("https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/amsterdam/2022/wijken-2022-zw-topo.json")
st_crs(kaart_wijk) = 4326

# tabel meest genoemde markt per wijk  -
markt_wijk<- tabel_list_marktnaam$tab_v1_wijk |>
  filter(v15_schoon != 'bezoekt markt, markt onbekend')|>
  group_by(v15_schoon, gbd_wijk_code, gbd_wijk_naam)|>
  summarise(aantal= sum(aantal))|>
  group_by(gbd_wijk_code, gbd_wijk_naam)|>
  mutate(aandeel=aantal/sum(aantal)*100)|>
  filter(v15_schoon!= 'bezoekt geen markt')|>
  filter(aandeel ==max(aandeel)) 

# samenvoegen tabel aan wijkenkaart -
kaart_markt <- kaart_wijk |>
  left_join(markt_wijk, by = c("code"= "gbd_wijk_code")) 

#plot de kaart
kaart_markt |>
  filter(aantal >8) |>
  ggplot(aes(fill=v15_schoon))+
  geom_sf(data = kaart_wijk,aes(fill= NULL), color = "white", size = 2.1)+
  geom_sf(color = "white", size = 2.1)+
  scale_fill_manual(name= NULL, values = markt_kleur)  +
  theme_ois_map(legend_position = "bottom")+
  guides(fill = guide_legend( title = NULL, ncol= 2))
ggsave("04 output tabellen/fig1_marktbez_wijk_d.svg", width = 7, height = 8)










### aandeel dat niet naar de markt gaat per wijk

niet_markt_wijk<- tabel_list_marktnaam$tab_v1_wijk |>
  group_by(v15_schoon, gbd_wijk_code, gbd_wijk_naam)|>
  summarise(aantal= sum(aantal))|>
  group_by(gbd_wijk_code, gbd_wijk_naam)|>
  mutate(aandeel=aantal/sum(aantal)*100)|>
  filter(v15_schoon == 'bezoekt geen markt') |>
  mutate(aandeel_kl=case_when(aandeel < 20                 ~ '0 tot 20%',
                              aandeel >= 20 & aandeel < 40 ~ '20 tot 40%',
                              aandeel >= 40 & aandeel < 50 ~ '40 tot 50%',
                              aandeel >= 50 & aandeel < 60 ~ '50 tot 60%',
                              aandeel >= 60                ~ '60% of hoger'))




#### kaarten ---- 

baby_blauw<- c("#009dec", "#65b0f0", "#93c3f4", "#b9d6f8", "#ddebfc")

kaart_niet_markt <- kaart_wijk |>
  left_join(niet_markt_wijk, by = c("code"= "gbd_wijk_code"))

kaart_niet_markt |>
  filter(aantal >8) |>
  ggplot(aes(fill=aandeel_kl))+
  geom_sf(data = kaart_wijk,aes(fill= NULL), color = "white", size = 2.1)+
  geom_sf(color = "white", size = 2.1)+
  theme_os_map(legend_position = "bottom")+
  scale_fill_manual(name= NULL, values = rev(baby_blauw)) +
  guides(fill = guide_legend())
ggsave("04 output tabellen/fig_geenmarkt_perwijk.png", width= 7, height = 5)




