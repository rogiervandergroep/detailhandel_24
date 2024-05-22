

### data wijken in Amsterdam ---

library(tidyverse)
library(openxlsx)
library(lubridate)
library(ggplot2)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

df_mastercard<- read.xlsx("00 ruwe data 2022 2023/Mastercard_sector7_maand_sd.xlsx")


df_mastercard <- df_mastercard |>
  mutate(
    kaarthouder = case_when(
      zeven_landen_indeling =='Continent: Europa Nederland' ~'Nederlandse betaalkaarten',
      TRUE ~ 'buitenlandse betaalkaarten'),
    
    kaarthouder = factor(kaarthouder, levels = c("Nederlandse betaalkaarten", "buitenlandse betaalkaarten")),
    
    maand_jaar  = lubridate::ym(glue::glue("{year} {month}")),
    
    gebieden    = case_when(
      gebied_naam == 'Centrum' ~ 'Centrum',
      gebied_naam %in% c('West', 'Zuid', 'Oost') ~ 'West, Zuid, Oost',
      TRUE ~ 'NW, Noord, ZO en Weesp')
    ) |>
  filter(sector != 'Overig',
         sector != 'Recreation')


my_summary <-function(x){
  
  x|>
    
    summarise(
      `bestedingen`=sum(normalised_euros),
      `transacties`=sum(normalised_transactions)
    )
}

my_percent <- function(x){
  
  x|>
    
    mutate(
      `aandeel bestedingen (%)`=`bestedingen`/sum(`bestedingen`)*100,
      `aandeel transacties (%)`=`transacties`/sum(`transacties`)*100
    )
}

my_index  <- function(x, tijdvar){
  
  x|>
    
    arrange({{tijdvar}}) |>
    
    mutate(
      `index bestedingen` = `bestedingen`/first(`bestedingen`)*100,
      `index transacties` = `transacties`/first(`transacties`)*100
    )
}

# totaal per maand
tabel_totaal <- bind_rows(
  
  # totaal per maand
  df_mastercard |>
    group_by(maand_jaar)|>
    my_summary()|>
    my_index(maand_jaar)|>
    add_column(kaarthouder = 'totaal',
               gebieden = 'Amsterdam',
               sector = 'totaal'),
  
  # totaal NL niet-NL
  df_mastercard |>
    group_by(maand_jaar, kaarthouder)|>
    my_summary()|>
    group_by(kaarthouder) |>
    my_index(maand_jaar)|>
    add_column(gebieden = 'Amsterdam',
               sector = 'totaal'),
  
  # totaal NL niet-NL
  df_mastercard |>
    group_by(maand_jaar, kaarthouder)|>
    my_summary()|>
    group_by(kaarthouder) |>
    my_index(maand_jaar)|>
    add_column(gebieden = 'Amsterdam',
               sector = 'totaal'),
  
  # totaal naar gebied
  df_mastercard |>
    group_by(maand_jaar, gebieden)|>
    my_summary()|>
    group_by(gebieden) |>
    my_index(maand_jaar)|>
    add_column(kaarthouder = 'totaal',
               sector = 'totaal'),
    
  # totaal NL niet-NL naar SD
  df_mastercard |> 
    group_by(maand_jaar, kaarthouder, gebieden)|>
    my_summary()|>
    group_by(kaarthouder, gebieden) |>
    my_index(maand_jaar)|>
    add_column(sector = 'totaal'),
  
  # totaal NL niet-NL naar SD
  df_mastercard |> 
    group_by(maand_jaar, kaarthouder, sector, gebieden)|>
    my_summary()|>
    group_by(kaarthouder, gebieden, sector) |>
    my_index(maand_jaar),
  
  # totaal  naar SD
  df_mastercard |> 
    group_by(maand_jaar, sector, gebieden)|>
    my_summary()|>
    group_by( gebieden, sector) |>
    my_index(maand_jaar)|>
    add_column(kaarthouder = 'totaal'),
  
  # totaal  naar sector
  df_mastercard |> 
    group_by(maand_jaar, sector)|>
    my_summary()|>
    group_by(sector) |>
    my_index(maand_jaar)|>
    add_column(gebieden = 'Amsterdam',
               kaarthouder = 'totaal'),
  
  # totaal kaarthouder naar sector
  df_mastercard |> 
    group_by(maand_jaar, kaarthouder, sector)|>
    my_summary()|>
    group_by(kaarthouder, sector) |>
    my_index(maand_jaar)|>
    add_column(gebieden = 'Amsterdam')

  
  )|>
  
  pivot_longer(cols= c(`bestedingen`: `index transacties`)) |>
  
  mutate(kaarthouder = factor(kaarthouder, levels= c("Nederlandse betaalkaarten", "buitenlandse betaalkaarten", "totaal")),
         gebieden    = factor(gebieden, levels = c("Centrum", "West, Zuid, Oost", "NW, Noord, ZO en Weesp", "Amsterdam")
         )
  )

dataverhaal <- bind_rows(
  
  df_mastercard |> 
    group_by(maand_jaar)|>
    my_summary()|>
    add_column(
      sector = 'totaal',
      gebied_code = 'Amsterdam',
      gebied_naam = 'Amsterdam'),
  
  df_mastercard |> 
    group_by(maand_jaar, gebied_code, gebied_naam)|>
    my_summary()|>
    add_column(sector = 'totaal'),

  df_mastercard |> 
    group_by(maand_jaar, sector)|>
    my_summary()|>
    add_column(
      gebied_code = 'Amsterdam',
      gebied_naam = 'Amsterdam'),

  df_mastercard |> 
    group_by(maand_jaar, gebied_code, gebied_naam, sector)|>
    my_summary())|>
  
  filter(gebied_naam != 'Other')|>
  
  pivot_longer(cols= c(`bestedingen`: `transacties`))
  

list_dataverhaal <- read_rds("04 output tabellen/tabel_dataverhaal.rds")

list_dataverhaal$transacties<- dataverhaal

write_rds(list_dataverhaal, "04 output tabellen/tabel_dataverhaal.rds")
  
  


my_plot <- function(data, xvar, yvar, vulvar, positie= "dodge"){
  
  data |>
    
    ggplot(aes(x={{xvar}}, 
               y=value,
               group={{vulvar}}))+
    
    geom_line(aes(color = {{vulvar}} ))+
    
    labs( x = NULL, y = NULL)+
    
    theme(
      axis.text.x  = element_text(vjust = 1, hjust = 1),
      plot.title   = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5))+
    
    scale_fill_manual(values=palettes_list$wild)+
    
    theme_os()+
    theme(strip.text = element_text(size = 12))
}

# figuur NL niet-NL
dataverhaal |>
  filter(sector != 'totaal',
         name %in% c('transacties' )) |>
  my_plot(xvar = maand_jaar,
          vulvar = sector) +
  scale_color_manual(values= wild_pal[c(1,3)])+
  scale_x_date(date_labels = "%b %y", date_breaks = "year")+
  facet_wrap(~ gebied_naam)
ggsave("04 output tabellen/fig_MA_01.png", height = 4, width = 9)


# figuur naar gebied
tabel_totaal |>
  filter(kaarthouder == 'totaal',
         sector != 'totaal',
         name %in% c('transacties' )) |>
  my_plot(xvar = maand_jaar, vulvar = sector, positie = 'stack') +
  facet_wrap(~ gebieden)+
  scale_color_manual(values= wild_pal)+
  scale_x_date(date_labels = "%B %y", date_breaks = "year")+
  guides(fill = guide_legend(nrow =3, reverse = F)) 
ggsave("04 output tabellen/fig_MA_02t.png", height = 5, width = 9)


# figuur naar sector
tabel_totaal |>
  filter(
    kaarthouder == 'totaal',
    sector != 'totaal',
    sector != 'Hotel-Motel',
    sector != 'Restaurant-cafés',
    name %in% c('transacties' )) |>
  
  my_plot(xvar = maand_jaar, vulvar = sector) +
  
  scale_x_date(
    date_labels = "%b %y", 
    breaks = as_date(c("2019-01-01", "2021-01-01", "2023-01-01"))) +
  scale_color_manual(values= wild_pal)+
facet_wrap(~gebieden)
ggsave("04 output tabellen/fig_MA_03.png", height = 4, width = 8)

