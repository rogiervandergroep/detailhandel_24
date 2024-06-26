
library(tidyverse)
library(openxlsx)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


### koopkrachtbinding toevoegen aan winkelgebieden ---

source("03 R scripts/scripts 02 analyse/00 02 script gebiedsindelingen BBGA.R")

# gebiedsindeling aan winkelgebieden koppelen -
ggw_gebieden <- geo_list |>
  filter(spatial_type == 'gebieden',
         spatial_date == '20220324')

stadsdelen <-  geo_list |>
  filter(spatial_type == 'stadsdelen',
         spatial_date == '20220324')

  
# koopkrachtbinding 2024 per gebied inlezen - 
kkb_geb_24 <- read_rds("04 output tabellen/tabel_kkb_geb_24.rds")|>
  map(\(x) add_column(
    x, periode = "veldwerkperiode 2022 2023", monitor = "monitor 2024"))

# koopkrachtbinding 2020 2024 per stadsdeel inlezen
kkb_sd_20_24 <-read_rds("04 output tabellen/tabel_kkb_sd_20tm24.rds")








# data samenvatten tot een koopkrachtbindingsgebied en vier afvloeiingsgebieden - --
my_samenvatting <- function(x, afzet_naam, herkomst_naam, herkomst_code) {
  
  x |>
    
    pivot_longer(
      cols = (where(is.numeric))
      )|>
    
    mutate(afzet_groep=case_when(
      {{afzet_naam}} == {{herkomst_naam}}  ~ 'binding gebied',
      {{afzet_naam}} == 'online'           ~ 'afvloeiing online',
      {{afzet_naam}} == 'MRA'              ~ 'afvloeiing overig Nederland',
      {{afzet_naam}} == 'overig NL'        ~ 'afvloeiing overig Nederland',
      {{afzet_naam}} == 'overig Nederland' ~ 'afvloeiing overig Nederland',
      TRUE                                 ~ 'afvloeiing overig Amsterdam')
      )|>
    
    mutate(afzet_groep=factor(
      afzet_groep, levels = c(
        'binding gebied',
        'afvloeiing overig Amsterdam',
        'afvloeiing overig Nederland',
        'afvloeiing online'))
      )|>
    
    group_by(
      periode, {{herkomst_code}} , {{herkomst_naam}},  name, afzet_groep)|>
    
    summarise(totaal= sum(value))|>
    
    filter(name %in% c("dagelijkse producten", "NDG totaal"))|>
    
    mutate(temporal_date = case_when(
      periode == 'veldwerkperiode 2019 2020' ~ '20200101',
      periode == 'veldwerkperiode 2020 2021' ~ '20220101',
      periode == 'veldwerkperiode 2022 2023' ~ '20240101')
      )|>
    
    mutate(measure = case_when(
      name == "dagelijkse producten" & afzet_groep == 'binding gebied'              ~ 'BHDG_BINDGEB_P',
      name == "dagelijkse producten" & afzet_groep == 'afvloeiing overig Amsterdam' ~ 'BHDG_BINDAMS_P',
      name == "dagelijkse producten" & afzet_groep == 'afvloeiing overig Nederland' ~ 'BHDG_BINDNL_P',
      name == "dagelijkse producten" & afzet_groep == 'afvloeiing online'           ~ 'EDG_BINDONLINE_P',
      
      name == "NDG totaal" & afzet_groep == 'binding gebied'              ~ 'BHNDG_BINDGEB_P',
      name == "NDG totaal" & afzet_groep == 'afvloeiing overig Amsterdam' ~ 'BHNDG_BINDAMS_P',
      name == "NDG totaal" & afzet_groep == 'afvloeiing overig Nederland' ~ 'BHNDG_BINDNL_P',
      name == "NDG totaal" & afzet_groep == 'afvloeiing online'           ~ 'BHNDG_BINDONLINE_P')
      )|>
    
    add_column(temporal_type = 'peildatum') |>
    rename(value = totaal)|>
    ungroup()
      

}

# samenvatting per gebied (alleen 2024)
kkb_gb_24_sam <- kkb_geb_24 |>
  map_df(\(x) my_samenvatting(
    x, afzet_naam = afzet_gebied_naam,
    herkomst_code = gbd_ggw_code,
    herkomst_naam = gbd_ggw_naam))|>
  left_join(ggw_gebieden, by = c("gbd_ggw_code"= "spatial_code"))|>
  rename (spatial_code = gbd_ggw_code) |>
  select(spatial_code,	spatial_name,	spatial_type,	spatial_date,	temporal_date,	temporal_type,	measure,	value)

# samenvatting per stadsdeel en amsterdam totaal (2020, 2022 en 2024)
kkb_sd_20_24_sam <- kkb_sd_20_24 |>
  map_df(\(x) my_samenvatting(
    x, afzet_naam = afzet_stadsdeel_naam, 
    herkomst_code = NULL,
    herkomst_naam = gbd_sdl_naam))|>
  left_join(stadsdelen, by = c("gbd_sdl_naam"= "spatial_name")) |>
  mutate(
    gbd_sdl_naam = case_when(gbd_sdl_naam == 'totaal Amsterdam' ~ 'Amsterdam',
                             TRUE ~ gbd_sdl_naam),
    spatial_code = replace_na(spatial_code, "0363"),
    spatial_type = replace_na(spatial_type, "Gemeente"),
    spatial_date = replace_na(spatial_date, "20220401")) |>
  rename (spatial_name = gbd_sdl_naam) |>
  select(spatial_code,	spatial_name,	spatial_type,	spatial_date,	temporal_date,	temporal_type,	measure,	value)

BBGA_binding <- bind_rows(kkb_gb_24_sam, kkb_sd_20_24_sam) |>
  filter(!is.na(value))

#write.csv2(BBGA_binding, "04 output tabellen/20240101_koopkrachtbinding_cijfers.csv")

pad_geb<- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/amsterdam/2022/gebieden-2022-geo.json"

gebieden_sf <- read_sf(pad_geb) 
  
gebieden_sf2 <- gebieden_sf |>
  select(naam, code, geometry)


gebieden_df <-gebieden_sf |>
  st_drop_geometry()|>
  select(code, stadsdeelCode, stadsdeelNaam)|>
  set_names(c("code", "herkomst_sd_code", "herkomst_sd_naam"))




my_join <- function(x){
  
  x |>
    
    left_join(
      gebieden_sf2, by = c("afzet_gebied_code" = "code"))|>
    
    st_as_sf() |>
    
    pivot_longer(
      cols= c(`dagelijkse producten`:`NDG totaal`)) |> 
    
    mutate(
      value_cut = os_cut(
        value, c(0,2,  5,  10,  15, 20, 30, 40, Inf),    suffix = '%')
      ) |>
    
    left_join(
      gebieden_df, by=c("gbd_ggw_code" = "code")
    )
      
    
  }


### alle data
kkb_geb_24_sf <- kkb_geb_24 |>
  map_df(\(x) my_join(x))

### overzicht binding per gebied
kkb_gb_24_sam_sf <- kkb_gb_24_sam |>
  
  left_join(gebieden_sf, by = c("spatial_code"="code")) |>
  filter(measure %in% c('BHNDG_BINDGEB_P','BHDG_BINDGEB_P' )) |>
  
  mutate(measure = case_when(
    measure == 'BHNDG_BINDGEB_P' ~ 'binding niet-dagelijkse producten',
    measure == 'BHDG_BINDGEB_P'  ~ 'binding dagelijkse boodschappen'))|>
  
  mutate(value_cut = os_cut(value, c(0, 5,  10, 15, 20, 30, 40, 60, 80, 100), suffix = '%')) |>
  st_as_sf() 
  



  

grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))

geom.text.size = 5
theme.size = (14/5) * geom.text.size


font <- "Amsterdam Sans"

# figuur overzicht dg

plot_gebied <- kkb_gb_24_sam_sf |>
  
  filter(
    spatial_code != 'GT21',
    spatial_code != 'GF06') |>
  
  ggplot()+
  
  geom_sf(
    data = gebieden_sf, fill= "#FFFFFF")+
  geom_sf(
    aes(fill = value_cut))+

  
  geom_sf_text(
    aes(
      label  = if_else(value > 5, as.character(round(value)),""),
      colour = if_else(value < 50, "#000000", "#FFFFFF")),
    size = 3.5,
    family = font)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = rev(palettes_list$blauw))  +
  scale_color_manual(name= NULL, values = c("#000000", "#FFFFFF"))  +
  guides(fill = guide_legend(ncol =1, reverse = T)) +
  theme(
    strip.text = element_text(size = 12, family = font),
    legend.position = "none",
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    panel.spacing = unit(0, "lines"), 
    plot.background = element_blank())+
  facet_wrap( ~ measure)
ggsave("04 output tabellen/fig_binding_geb.png", width = 7, height = 3)








# koopkrachtbinding per gebied binnen een stadsdeel

my_plot<- function (x , sd, prodgroep) {
  
  x |>
    filter(herkomst_sd_code == sd,
           name == prodgroep) |>
    ggplot()+
    
    geom_sf(
      aes(fill = value_cut))+
    
    geom_sf_text(
      aes(label = if_else(
        value > 1 ,as.character(round(value)),"")), 
        family= font, size= 3)+
    
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    scale_fill_manual(name= NULL, values = rev(palettes_list$blauw))  +
    guides(fill = guide_legend(ncol =1, reverse = T)) +
    theme(
      strip.text = element_text(size = 11),
      text = element_text(family = font),
      legend.position = "right",
      axis.line = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(), 
      panel.background = element_blank(), 
      panel.grid = 
        element_blank(), 
        panel.spacing = unit(0, "lines"), 
        plot.background = element_blank())+
    facet_wrap( ~ gbd_ggw_naam)
  
  
  
}


plot_centrum_ndg <- kkb_geb_24_sf |>
  my_plot ("A", "NDG totaal")

plot_centrum_ndg



plot_zo_ndg <- kkb_geb_24_sf |>
  my_plot ("T", "NDG totaal")

plot_zo_ndg

















