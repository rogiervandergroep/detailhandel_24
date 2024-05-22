


source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

achtergrondvar<- c(
  "respdef",
  "opleiding_klas", "inkomen_klas", 
  "huishouden_klas", "geslacht", 
  "leeftijd_klas", 
  "gbd_brt_code", "gbd_brt_naam",
  "gbd_wijk_code","gbd_wijk_naam",
  "gbd_ggw_code", "gbd_ggw_naam",
  "gbd_sdl_code", "gbd_sdl_naam")


# data gemaakt in 'script 20 01 bezoek markten.R' inlezen
data_markt_def <- read_rds("03 tussentijds/data_markt_def.RDS") |>
  map(\(x) filter(x, !is.na(v2), v2>0)) |>
  map(\(x) select(x, v2, starts_with("v3"), all_of(achtergrondvar)))|>
  map(\(x) mutate(x, across(starts_with("v3"), ~ replace_na(., 0))))|>
  map(\(x) set_names(x, 
     c("geschat bedrag", "supermarkt", "andere winkels", 
       "markt", "online", "overig", achtergrondvar))) |>
  map(\(x) mutate(x, overig = (100 - (supermarkt + `andere winkels`+ markt + online))))

data_markt_long <- data_markt_def |>
  map(\(x) pivot_longer(x, cols=c(`geschat bedrag`, supermarkt:overig) ))

  


jaren <- c("monitor 2024" , "monitor 2022", "monitor 2020")

v2_levels <- c("geschat bedrag", "supermarkt", "andere winkels", "markt", "online", "overig")

sd_levels<- c("Centrum", "Westpoort", "West", "Nieuw-West", "Zuid", 
              "Oost", "Noord", "Weesp", "Zuidoost", "Amsterdam totaal", "Amsterdam")

my_function <- function (x, var=NULL) {
  
  x |>
  
    group_by(name, {{var}})|>
    summarise(value =round(mean(value))) |>
    mutate(name=factor(name, levels = v2_levels)) |>
    
    pivot_wider(values_from = value, names_from = name) |>
    mutate(overig=100- (supermarkt + `andere winkels`+ markt + online)) |>
    pivot_longer(where(is.numeric))
    
  
}

my_mutate<- function(x) {
  
  x |>
    mutate (value = case_when(
    gbd_sdl_naam == 'Weesp' & monitor %in% c('monitor 2022', 'monitor 2020') ~ NA,
    TRUE ~ value))  |>
    filter(!is.na(gbd_sdl_naam), gbd_sdl_naam != 'Westpoort') |>
    mutate(gbd_sdl_naam=factor(gbd_sdl_naam, levels = sd_levels))
  
  
}


### FIGUREN ----





# naar stadsdeel en naar monitor
tabel_ams_sd  <- bind_rows(
  
  # amsterdam totaal
  data_markt_long |>
    map    (\(x) my_function(x)) |>
    map2_df(jaren, \(x,y) add_column(x, monitor = y)) |>
    add_column(gbd_sdl_naam = 'Amsterdam'),
  
  # stadsdelen
  data_markt_long |>
    map    (\(x) my_function(x, var = gbd_sdl_naam)) |>
    map2_df(jaren, \(x,y) add_column(x, monitor = y)) 
  )|> my_mutate()
  


# naar stadsdeel en inkomen 
tabel_ink  <- bind_rows(
  
  data_markt_long |>
    map    (\(x) my_function(x, var = inkomen_klas)) |>
    map2_df(jaren, \(x,y) add_column(x, monitor = y)) |>
    add_column(gbd_sdl_naam = 'Amsterdam'), 
  
  data_markt_long |>
    map    (\(x) my_function(x, var = across(c("inkomen_klas", "gbd_sdl_naam")))) |>
    map2_df(jaren, \(x,y) add_column(x, monitor = y))
  ) |> 
  my_mutate() |>
  filter( inkomen_klas != 'inkomen onbekend')
  
#huishouden en inkomen
tabel_ink_hh  <- bind_rows(
  
  data_markt_long |>
    map    (\(x) my_function(x, var = huishouden_klas)) |>
    map2_df(jaren, \(x,y) add_column(x, monitor = y)) |>
    add_column(inkomen_klas = 'Amsterdam'), 
  
  data_markt_long |>
    map    (\(x) my_function(x, var = across(c("inkomen_klas", "huishouden_klas")))) |>
    map2_df(jaren, \(x,y) add_column(x, monitor = y))
) 

my_plot <- function (x){
  
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
      
      
      y     = fct_relevel(fct_rev(gbd_sdl_naam), "Amsterdam"),
      group = fct_relevel(fct_reorder(name, value), "overig"),
      x     = value,
      label = value)
      
      )+
      
    geom_col(aes(
      fill = fct_relevel(fct_reorder(name, value), "overig")))+
    
    geom_text(
      aes(
        label = if_else(value > 5,as.character(round(value)),""),
        color = fct_relevel(fct_reorder(name, value), "overig")),
      
      position = position_stack(vjust =0.5),
      family = font, 
      lineheight = .8)+
    
    labs(
      title=NULL, x=NULL, y = NULL) +
    
    theme_os2()+
    
    scale_fill_manual(
      name= NULL, values = blauw_pal[c(8, 2,3,5,6,7,8)])+
    
    scale_color_manual(
      name= NULL, values = label_col[c(8, 2,3,5,6,7,8)])+
    
    guides(
      fill = guide_legend(nrow =1, reverse = T), 
      colour = "none") 
}

# figuur aandeel supermarkt online markt amsterdam monitor 2020 2022 en 2024
tabel_ams_sd |>
  filter(name != 'geschat bedrag') |>
  my_plot()+
  facet_wrap(~ monitor)
ggsave("04 output tabellen/fig_v2_aand_sup_sd.png", width = 7, height = 3)  

# figuur aandeel supermarkt 2024 inkomen alleen amsterdam

tabel_ink|>
  filter(
    monitor == 'monitor 2024',
    name != 'geschat bedrag',
    inkomen_klas != 'inkomen onbekend',
    !is.na(inkomen_klas)) |>
  
  my_plot()+
  facet_wrap(~ fct_relevel(inkomen_klas, "inkomen laag", "inkomen midden", "inkomen hoog"))
ggsave("04 output tabellen/fig_v2_aand_sup_ink.png", width = 7, height = 3)  


my_plot2<- function(x, yvar){
  
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
  
  
  x |>
    
    ggplot(aes(
      y = {{yvar}},
      x = value))+
    
    geom_col(fill= blauw_pal[2])+
    
    geom_text(aes(
      label = if_else(value > 3,glue::glue("€ {as.character(round(value))},-"),"")), 
      position = position_stack(vjust =0.5),
      family=font,
      color = "white",
      lineheight=.8)+
    
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os2()+ 
    scale_fill_manual(name= NULL, values = blauw_pal[2])  +
    guides(fill = guide_legend(reverse = T))
  
  
  
}

# figuur geschat bedrag per sd
tabel_ams_sd|>
  filter(
    name == 'geschat bedrag') |>
  
  my_plot2(fct_relevel(fct_rev(gbd_sdl_naam), "Amsterdam")) +
  facet_wrap(~ monitor)
ggsave("04 output tabellen/fig_v2_kosten_sd.png", width = 7, height = 3)  


# figuur geschat bedrag per huishoudtype

tabel_def<- bind_rows(
  
  tabel_ams_sd |>
    add_column(
      huishouden_klas = 'Amsterdam',
      inkomen_klas = 'Amsterdam'), 
  
  tabel_ink_hh  |>
    add_column(
      gbd_sdl_naam = 'Amsterdam')
)

tabel_def|>
  filter(
    gbd_sdl_naam == 'Amsterdam',
    huishouden_klas != 'overig, of huishoudtype onbekend',
    inkomen_klas == 'Amsterdam',
    name == 'geschat bedrag') |>
  
  my_plot2(fct_rev(monitor)) +
  facet_wrap(~ fct_relevel(fct_reorder(huishouden_klas, value), 'Amsterdam', after = Inf))
ggsave("04 output tabellen/fig_v2_kosten_hh.png", width = 7, height = 3)  



tabel_ink|>
  filter(
    monitor == 'monitor 2024',
    name == 'geschat bedrag',
    inkomen_klas != 'inkomen onbekend') |>
  
  my_plot2(fct_relevel(fct_rev(gbd_sdl_naam), "Amsterdam"))+
  facet_wrap(~ fct_relevel(inkomen_klas, "inkomen laag", "inkomen midden", "inkomen hoog"))
ggsave("04 output tabellen/fig_v2_kosten_ink.png", width = 7, height = 3)  

tabel_ink_hh|>
  filter(
    monitor == 'monitor 2024',
    name == 'geschat bedrag',
    inkomen_klas != 'inkomen onbekend',
    huishouden_klas != 'overig, of huishoudtype onbekend') |>
  
  my_plot2(fct_relevel(inkomen_klas, "Amsterdam", "inkomen hoog", "inkomen midden", "inkomen laag"))+
  facet_wrap(~ fct_reorder(huishouden_klas, value))
ggsave("04 output tabellen/fig_v2_kosten_ink_huish.png", width = 7, height = 3)  





 

