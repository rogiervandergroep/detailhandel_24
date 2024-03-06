


library(tidyverse)


grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))
font <- "Corbel"

# https://os-amsterdam.gitlab.io/datavisualisatie-onderzoek-en-statistiek/
# source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


theme_os     <- function(orientation="vertical", legend_position = "bottom"){
  
  grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))
  font <- "Corbel"
  
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
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
  
  return(theme)
}
theme_os_map <- function(legend_position = c(0, 0)){
  
  grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))
  font <- "Corbel"
  
  
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(), 
      panel.background = element_blank(), 
      panel.grid = element_blank(), panel.spacing = unit(0, "lines"), plot.background = element_blank(), 
      legend.justification = c(0, 0), legend.position = legend_position,
      plot.caption = ggplot2::element_text(family = font, size = 12),
      plot.subtitle = ggplot2::element_text(family = font, size = 12),
      legend.text = ggplot2::element_text(family = font, size = 12),
      plot.title = ggplot2::element_text(family = font, lineheight = 1.2, size = 12),
      legend.title = ggplot2::element_text(family = font, lineheight = 1.2, size = 12),
      panel.border = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.text = ggplot2::element_text(color = "black", family = font, face = "bold", size = 12)
    ) 
  
  
}

blauw_pal <- c("#004699", "#3858a4", "#566bb0", "#707ebb", "#8992c6", "#a1a7d2", "#b8bcdd", "#d0d2e8", "#e7e8f4")
wild_pal  <- c("#004699", "#009de6", "#53b361", "#bed200", "#ffe600", "#ff9100", "#ec0000")
fruitig   <- c("#a00078", "#e50082", "#009de6", "#fb9bbe", "#d48fb9", "#a4ccf3", "#ffd8e5", "#efd2e3", "#dceafa")

load("03 tussentijds/tabellen_markten_def.RData")
load("03 tussentijds/data_markt_def.RDS")

# Samenvoegen Amsterdam totaal met Stadsdelen - 

tabel_AMS_SD<- bind_rows(
  
  tabel_list_geenmarkt$tab_v1_ams |>
    add_column(gbd_sdl_code='AMS',
               gbd_sdl_naam='Amsterdam'), 
  
  tabel_list_geenmarkt$tab_v1_sd)


my_plot <- function (x, groupvar, fillvar = marktbezoek ){
  
  x |>
    
    ggplot(aes(x= aandeel, y= {{groupvar}}, fill= {{fillvar}} ))+
    geom_col()+
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = blauw_pal[c(7,2)]) +
    guides(fill = guide_legend(reverse = T))
  
  
}

my_plot_sd_ink  <- function (x, sd) {
  
  # meest bezochte markt per stadseel naar inkomen
  x |>
    
    mutate(inkomen_n=factor(inkomen_n, levels= c("laag", "midden", "hoog", "totaal")))|>
    
    filter(gbd_sdl_naam == sd,
           jaar == "jaar 2023",
           !is.na(inkomen_n))|>
    
    mutate(v15_schoon=fct_lump_n(v15_schoon, n= 5, w= aandeel, other_level = "overig")) |>
    
    ggplot(aes(x= aandeel,
               fill= blauw_pal[1] ,
               y= fct_relevel(fct_reorder(v15_schoon, aandeel), "bezoekt geen markt", "overig")))+
    
    geom_col(position="dodge")+
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = wild_pal)  +
    guides(fill = guide_legend(nrow =1, reverse = T)) +
    facet_wrap(~ inkomen_n, ncol =4)+
    theme(legend.position = 'none')
  
  
}

my_plot_sd_jaar <- function (x, sd, filvar) {
  
  # meest bezochte markt per stadseel naar inkomen
  
  x |>
    
    filter(gbd_sdl_naam == sd)|>
    
    mutate(v15_schoon=fct_lump_n(v15_schoon, n= 6, w= aandeel, other_level = "overig")) |>
    
    ggplot(aes(x= aandeel,
               fill= {{filvar}} ,
               y= fct_relevel(fct_reorder(v15_schoon, aandeel), "bezoekt geen markt", "overig")))+
    
    geom_col(position="dodge")+
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = wild_pal) +
    guides(fill = guide_legend(nrow =1, reverse = T))
  
  
}

my_plot_sd_lfk  <- function (x, sd) {
  
  # meest bezochte markt per stadseel naar inkomen
  x |>
    
    mutate(
      leefklas=factor(
        leefklas, 
        levels= c("tot en met 34 jaar", "35 tot en met 54 jaar",      
                  "55 jaar tot en met 67 jaar", "68 jaar of ouder",
                  'onbekend',"totaal"),
        labels= c("tm 34 jaar","35-54 jaar","55-67 jaar",
                  "68 jaar e.o", "onbekend", "totaal")))|>
    
    filter(gbd_sdl_naam == sd,
           jaar == "jaar 2023",
           leefklas!= 'onbekend',
           !is.na(leefklas))|>
    
    mutate(v15_schoon=fct_lump_n(v15_schoon, n= 5, w= aandeel, other_level = "overig")) |>
    
    ggplot(aes(x= aandeel,
               fill= blauw_pal[1] ,
               y= fct_relevel(fct_reorder(v15_schoon, aandeel), "bezoekt geen markt", "overig")))+
    
    geom_col(position="dodge")+
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, 
                      values = wild_pal)  +
    guides(fill = guide_legend(nrow =1, reverse = T)) +
    facet_wrap(~ leefklas, ncol =4)+
    theme(legend.position = 'none')
  
  
}

my_plot_sd_geb  <- function (x, sd) {
  
  # meest bezochte markt per stadseel naar inkomen
  x |>
    
    filter(gbd_sdl_naam == sd,
           jaar == "jaar 2023")|>
    
    mutate(v15_schoon=fct_lump_n(v15_schoon, n= 5, w= aandeel, other_level = "overig")) |>
    
    ggplot(aes(x= aandeel,
               fill= blauw_pal[1] ,
               y= fct_relevel(fct_reorder(v15_schoon, aandeel), "bezoekt geen markt", "overig")))+
    
    geom_col(position="dodge")+
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = wild_pal)  +
    guides(fill = guide_legend(nrow =1, reverse = T)) +
    facet_wrap(~ gbd_ggw_naam, ncol =4)+
    theme(legend.position = 'none')
  
  
}


list_md_zo<- list()

# marktbezoek amsterdam versus Zuidoost
list_md_zo$fig_ams_zo<-tabel_AMS_SD |>
  filter(gbd_sdl_naam %in% c('Zuidoost', 'Amsterdam'))|>
  my_plot(fct_rev(jaar), marktbezoek)+
  facet_wrap(~ gbd_sdl_naam)+
  geom_text(aes(label = round(aandeel), family= "Corbel"),
            position = position_stack(vjust = 0.5),
            color= "#FFFFFF" )


# plot Zuidoost naar jaar -
list_md_zo$fig_mrkt_zo_jaar<- 
  my_plot_sd_jaar(tabel_list_marktnaam$tab_v1_sd, 
                  sd= "Zuidoost", 
                  filvar = fct_rev(jaar))

# plot Zuidoost naar leeftijd -
list_md_zo$fig_mrkt_zo_lfk<-
  my_plot_sd_lfk(tabel_list_marktnaam$tab_v1_sd_lft, 
                 sd= "Zuidoost")

# plot Zuidoost naar 22 geb -
list_md_zo$fig_mrkt_zo_geb<-tabel_list_marktnaam$tab_v1_geb|>
  filter(gbd_ggw_naam != 'Bijlmer-West')|>
  my_plot_sd_geb(sd="Zuidoost")

# plot Zuidoost naar inkomen -
list_md_zo$fig_mrkt_zo_ink <-
  my_plot_sd_ink(tabel_list_marktnaam$tab_v1_sd_ink, sd= "Zuidoost")

library(sf)
### plot kaart met meest genoemde markt per gebied -

# wijkenkaart van amsterdam - 
kaart_wijk <- read_sf("https://os-amsterdam.gitlab.io/datavisualisatie-onderzoek-en-statistiek/geo/wijken-2022-zw-topo.json")
st_crs(kaart_wijk) = 4326

# tabel meest genoemde markt per wijk  -
markt_wijk<- tabel_list_marktnaam$tab_v1_wijk |>
  filter(jaar %in% c('jaar 2023', 'jaar 2021')) |>
  group_by(v15_schoon, gbd_wijk_code, gbd_wijk_naam)|>
  summarise(aantal= sum(aantal))|>
  group_by(gbd_wijk_code, gbd_wijk_naam)|>
  mutate(aandeel=aantal/sum(aantal)*100)|>
  filter(v15_schoon!= 'bezoekt geen markt')|>
  filter(aandeel ==max(aandeel)) 

# samenvoegen tabel aan wijkenkaart -
kaart_markt <- kaart_wijk |>
  left_join(markt_wijk, by = c("code"= "gbd_wijk_code")) |>
  filter(sdl_naam == 'Zuidoost')

#plot de kaart
list_md_zo$kaart_mrkt_zo_wijk<- kaart_markt |>
  filter(aantal >5) |>
  ggplot(aes(fill=v15_schoon))+
  geom_sf(data = kaart_wijk[kaart_wijk$sdl_naam=='Zuidoost',],aes(fill= NULL))+
  geom_sf()+
  theme_os_map(legend_position = "bottom")+
  scale_fill_manual(name= NULL, values = wild_pal)  +
  guides(fill = guide_legend(nrow = 2))


### redenen om niet de markt te bezoeken ---

load("03 tussentijds/tabel_geen_totaal.RDS")

my_plot2 <- function (x, naam,  filvar) {
  
  x |>
    
    filter(
      achtergrondvar_name %in% naam,
      jaar == 'jaar 2023',
      gbd_sdl_naam %in% c("Amsterdam", "Zuidoost"))|>
    
    ggplot(aes(x= aandeel, 
               y= fct_relevel(fct_lump(fct_reorder(value, aandeel), w= aandeel, 6, other_level = "overig"), "overig", "anders"),
               fill = {{filvar}} ))+
    geom_col(position = 'dodge')+
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = wild_pal) +
    guides(fill = guide_legend(reverse = T, nrow = 2))
  
}  # alleen 2023 en alleen AMS en ZO


# aandeel dat geen markt bezoekt -
list_md_zo$plot_geenbezoek_zo<- tabel_geen_totaal   |> 
  my_plot2(naam = 'totaal', filvar= gbd_sdl_naam)+  
  scale_fill_manual(name= NULL, values = blauw_pal[c(7,2)]) +
  guides(fill = guide_legend(reverse = T, nrow = 1))

# inkomen
list_md_zo$plot_geenbezoek_zo_ink<-tabel_geen_totaal   |> 
  filter(gbd_sdl_naam=='Zuidoost')|>
  mutate(achtergrondvar_type=factor(achtergrondvar_type, levels=  c("laag", "midden", "hoog")))|>
  my_plot2(naam = c('inkomen'), filvar= fct_rev(achtergrondvar_type))+  
  guides(fill = guide_legend(reverse = T, nrow = 1))

lev_lft <- c("tot en met 34 jaar", 
             "35 tot en met 54 jaar", 
             "55 jaar tot en met 67 jaar",
             "68 jaar of ouder",
             "onbekend")


# leeftijd
list_md_zo$plot_geenbezoek_zo_lft<-tabel_geen_totaal   |> 
  filter(gbd_sdl_naam=='Zuidoost', 
         achtergrondvar_type != 'onbekend')|>
  mutate(achtergrondvar_type=factor(achtergrondvar_type, levels=  lev_lft))|>
  my_plot2(naam = c('leefktijdsklasse'), filvar= fct_rev(achtergrondvar_type))


# bedrag per stadsdeel en totaal amsterdam -

# NB : Bij Anton de Komplein is een resp. met 760 euro, deze is verwijderd uit de data

v16_bedrag <- bind_rows(
  
  data_markt_def |>
  filter(
    v16 < 400,
    v15_schoon != 'bezoekt geen markt',
    !is.na(gbd_sdl_naam)) |>
  group_by(gbd_sdl_naam, jaar)|>
  summarise(aantal = n(),
            bedrag = mean(v16, na.rm=T)),
  
  data_markt_def |>
  filter(
    v16 < 400,
    v15_schoon != 'bezoekt geen markt') |>
  group_by( jaar)|>
  summarise(aantal = n(),
            bedrag = mean(v16, na.rm=T))|>
  add_column(gbd_sdl_naam='Amsterdam')
)

# bedrag per markt in zuidoost -

markten_zo <- c("Anton de Kompleinmarkt", 
                "Reigersbosmarkt",
                "Ganzenhoefmarkt", 
                "Kraaiennestmarkt")

v16_bedrag_markt <- data_markt_def |>
  filter(
    v16 < 400,
    v15_schoon %in% markten_zo)|>
  group_by(v15_schoon, jaar)|>
  summarise(aantal = n(),
            bedrag = mean(v16, na.rm=T))

list_md_zo$fig_v16_bedrag_markt <-
  v16_bedrag_markt|>
  ggplot(aes(x= jaar,y= bedrag, group=v15_schoon, color=v15_schoon))+
  geom_line(size= .9)+
  geom_point(size= 2)+
  labs(title=NULL, x=NULL, y=NULL) +
  theme_os()+ 
  scale_color_manual(name=NULL, values = wild_pal)  +
  guides(color = guide_legend(reverse = T, nrow = 2))

list_md_zo$fig_v16_bedrag_zo <-
  v16_bedrag|>
  filter(gbd_sdl_naam %in% c("Zuidoost", "Amsterdam"))|>
  ggplot(aes(x= jaar,y= bedrag, group=gbd_sdl_naam, color=gbd_sdl_naam))+
  geom_line(size=.9)+
  geom_point(size = 2)+
  labs(title=NULL, x=NULL, y=NULL) +
  theme_os()+ 
  scale_color_manual(name= NULL, values = wild_pal[c(1,4)]) +
  guides(color = guide_legend(reverse = T, nrow = 1))

### bezoekfrequentie zuidoost ---

load("G:/OIS/Projecten/lopende Projecten/22155 Monitor Markten 2022/data/05 OUTPUT TABELLEN/tab_bez_pas.RData")


markten_zo<- c("Anton de Komplein", "Reigersbos", "Kraaiennest", "Ganzenhoef")

# verander jaar 206 in 2017 
my_mutate <- function(x)  {
  
  x |>
    mutate(jaar=case_when(jaar == 'jaar 2016' ~ '2017',
                          jaar == 'jaar 2022' ~ '2022'))
}

# v1: bezoekfrequentie
list_md_zo$v1_freq<- verz_bez_pas_long$xls_tab_v01z |>
  filter(markt %in% markten_zo) |>
  my_mutate()|>
  ggplot(aes(x= perc, y= fct_rev(jaar), fill = fct_rev(v1)))+
  geom_col()+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = rev(blauw_pal[c(1,3,5,6,7,9)])) +
  facet_wrap(~markt)+
  guides(fill = guide_legend(reverse = T, nrow = 2))

# v2: belangrijkste reden om markt te bezoeken 
list_md_zo$v2_reden <- verz_bez_pas_long$xls_tab_v02 |>
  filter(markt %in% markten_zo,
         jaar == 'jaar 2022',
         v2_reden != 'geen antwoord') |>
  
  ggplot(aes(x= perc, 
             y= fct_relevel(fct_reorder(v2_reden, perc), "anders"), 
             fill= blauw_pal[1]))+
  geom_col(position = "dodge")+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os(legend_position = "none")+ 
  scale_fill_manual(name= NULL, values = blauw_pal) +
  facet_wrap(~markt,  nrow = 1)

# v3: voor welke producten komt men op de markt -
list_md_zo$v3_prod<- verz_bez_pas_long$xls_tab_v03 |>

  mutate(name=str_sub(name, 6),
         name=case_when(name == "groentenfruit" ~ "groente en fruit",
                        name == "etenswaren_anders" ~ "andere etenswaren",
                        name == "huish_art" ~ "huishoudelijke artikelen",
                        name == "broodbanket" ~"brood en banket",
                        name == "bloementuinplanten" ~  "bloemen en tuinplanten",
                        TRUE ~  name)) |>
           
  filter(markt %in% markten_zo,
         jaar == 'jaar 2022') |>
  
  ggplot(aes(x= aandeel, 
             y= fct_relevel(fct_reorder(name, aandeel), "anders"),  
             fill= blauw_pal[1]))+
  geom_col(position = "dodge")+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os(legend_position = "none")+ 
  scale_fill_manual(name= NULL, values = blauw_pal) +
  facet_wrap(~markt,  nrow = 1)


# v3: voor welke producten komt men op de markt -
list_md_zo$v3_prod<- verz_bez_pas_long$xls_tab_v03 |>
  
  mutate(name=str_sub(name, 6),
         name=case_when(name == "groentenfruit" ~ "groente en fruit",
                        name == "etenswaren_anders" ~ "andere etenswaren",
                        name == "huish_art" ~ "huishoudelijke artikelen",
                        name == "broodbanket" ~"brood en banket",
                        name == "bloementuinplanten" ~  "bloemen en tuinplanten",
                        TRUE ~  name)) |>
  
  filter(markt %in% markten_zo,
         jaar == 'jaar 2022') |>
  
  ggplot(aes(x= aandeel, 
             y= fct_relevel(fct_reorder(name, aandeel), "anders"),  
             fill= blauw_pal[1]))+
  geom_col(position = "dodge")+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os(legend_position = "none")+ 
  scale_fill_manual(name= NULL, values = blauw_pal) +
  facet_wrap(~markt,  nrow = 1)

# v7: oordeel warenmarkten ---

list_md_zo$v7_oordeel<- verz_bez_pas_long$xls_tab_v07 |>
  filter(markt %in% markten_zo) |>
  my_mutate()|>
  ggplot(aes(x= perc, y= fct_rev(jaar), fill = fct_rev(v7)))+
  geom_col()+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = rev(blauw_pal[c(1,3,5,6,7,9)])) +
  facet_wrap(~markt)+
  guides(fill = guide_legend(reverse = T, nrow = 2))


# v7: waarom ontevreden 

list_md_zo$v7_reden_slecht <- verz_bez_pas_long$xls_tab_v07_redenm |>
  filter(markt %in% markten_zo,
         jaar== 'jaar 2022') |>
  pivot_wider(values_from = aandeel, names_from = markt, values_fill = 0 , - aantal) |>
  pivot_longer(where(is.numeric), names_to = "markt", values_to = "aandeel") |>
  ggplot(aes(x= aandeel, 
             y= fct_relevel(fct_reorder(reden_label, aandeel)),  
             fill= wild_pal[1]))+
  geom_col()+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os(legend_position = 'none')+ 
  scale_fill_manual(name= NULL, values = wild_pal)+
  facet_wrap(~markt, nrow=1)

# v10 : rapportcijfers

list_md_zo$v10_rap <- verz_bez_pas_long$xls_tab_v10 |>
  filter(markt %in% markten_zo,
         groep == 'bezoekers en passanten')|>
  pivot_wider(values_from = gemiddelde, names_from = markt, - aantal) 
  

list_md_zo$v10_rap |>
  filter(jaar == 'jaar 2022') |>
  ungroup()|>
  select(rapportcijfer, `Anton de Komplein`: Reigersbos)

## knelpunten en sterke punten per markt

openantwoorden <- bind_rows(
  
  verz_bez_pas_long$xls_tab_v11 |>
    filter(markt %in% markten_zo) |>
    select(markt,jaar, groep, open_antwoord,vraag_omschrijving ) , 
  
  verz_bez_pas_long$xls_tab_v12 |>
    filter(markt %in% markten_zo) |>
    select(markt,jaar, groep, open_antwoord,vraag_omschrijving )
)

save(list_md_zo,file= "03 R scripts/scripts Markdown Zuidoost/list_md_zo.RDS")

write.xlsx(openantwoorden , "04 output tabellen/openantwoorden_zuidoost.xlsx")









