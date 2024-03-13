

library(tidyverse)
library(openxlsx)


source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))

font <- "Amsterdam Sans"


### tabel 1 eenvoudig ---
tabel <- read.xlsx("00 ruwe data 2022 2023/tabel abr_polis detailhandel.xlsx") |>
  pivot_longer(cols= c(dagelijks:amsterdam))

opl_levels <- c('vmbo-geschoold', 'mbo-geschoold', 'hbo, wo-geschoold', 'opleiding onbekend')
lft_levels <- c('tot 18 jaar','18 tot en met 34 jaar','35 tot en met 55 jaar','55 tot en met 67 jaar','68 jaar of ouder','leeftijd onbekend')
etn_levels <- c("autochtoon",  "overig westers","Marokko","Turkije","Suriname",        
                "Antillen en Aruba" ,"overig niet-westers","onbekend")

woon_levels <- c(
  'Amsterdam',
  'overig MRA',
  'woonplaats buiten MRA')

                 
                 
## andere tabellen werkenden obv CBS data

cbs_data <- list()

pad <- "00 ruwe data 2022 2023/tabel_abrpolis_detailhandel.xlsx"


cbs_data$etn <- read.xlsx(pad, sheet = "sec_etn")|>
  add_column(indicator = "etnische achtergrond") |>
  rename(categorieen = etngrp_naam) 

unique(cbs_data[["etn"]][["categorieen"]])

cbs_data$opl <- read.xlsx(pad, sheet = "sec_opl")|>
  add_column(indicator = "opleiding")|>
  rename(categorieen = opl3) |>
  filter(!is.na(categorieen)) |>
  mutate(categorieen =case_when(
    
    categorieen == 'laag'     ~ 'vmbo-geschoold',
    categorieen == 'midden'   ~ 'mbo-geschoold', 
    categorieen == 'hoog'     ~ 'hbo, wo-geschoold',
    categorieen == 'onbekend' ~ 'opleiding onbekend')
  ) 

cbs_data$ges <- read.xlsx(pad, sheet = "sec_gesl")|>
  add_column(indicator = "geslacht")|>
  rename(categorieen = gbageslacht) |>
  
  mutate(categorieen = case_when(
    categorieen == '1' ~ 'man',
    categorieen == '2' ~ 'vrouw',
    is.na(categorieen) ~ 'onbekend')
    )

cbs_data$lfk <- read.xlsx(pad, sheet = "sec_lfk")|>
  add_column(indicator = "leeftijd")|>
  rename(categorieen = leef_klas)|>
  mutate(categorieen = replace_na(categorieen, 'leeftijd onbekend'))

cbs_data$woonpl <- read.xlsx(pad, sheet = "sec_woonpl")|>
  rename(categorieen = woondeelregio)|> 
  add_column(indicator = "woonregio") |>
  
  mutate(categorieen = case_when(
    categorieen == 'Amsterdam' ~ 'Amsterdam',
    categorieen == 'woonplaats buiten MRA' ~ 'woonplaats buiten MRA',
    TRUE ~ 'overig MRA')
  )



# uurloon 

cbs_data$totaal <- read.xlsx(pad, sheet = "sec")

my_uurloon_plot <- function (x) {
  
  x |>
    
    ggplot(
      aes(y = fct_rev(sector), 
          x = loonperuur, 
          fill = fct_rev(jaar)))+

    geom_col(
      position = 'dodge')+
    
    geom_text(
      aes(label = glue::glue("{loonperuur},-")), 
      position = position_dodge(  width = .9 ),
      hjust = -.7,
      family = font)+

    labs(
      title=NULL, x=NULL, y = NULL) +
    
    theme_os()+ 
    theme(
      strip.text = element_text(size = 12, family = font))+
    
    scale_fill_manual(
      name= NULL, values = blauw_pal[c(1,5)])+
    
    guides(
      fill = guide_legend(reverse = T))
  
}



opl_levels <- c('vmbo-geschoold', 'mbo-geschoold', 'hbo, wo-geschoold', 'totaal')

tabel_uurloon <- bind_rows(
  
  cbs_data$totaal |>
    add_column(
      categorieen = 'totaal'),
  
  cbs_data$opl |>
    filter(
      categorieen != 'opleiding onbekend',
      categorieen != 'onbekend')
  
  ) |>
  
  filter(sector != 'overige sectoren')|>
  mutate(jaar= str_remove(jaar, "banen_bedrijven_dec"))|>
  filter(jaar %in% c('2016', '2022')) |>
  mutate(categorieen = factor(categorieen, levels= opl_levels))
                              
                              

                              

tabel_uurloon |> 
  my_uurloon_plot()+
  facet_wrap(~ categorieen) 

ggsave("04 output tabellen/fig1_abrpolis_uurloon1.png", width = 9, height = 5)





# aandeel werkenden 
my_perc <- function (x){
  
  x |>    
    
    filter(
      sector != 'overige sectoren') |>
    
    group_by(
      jaar, sector) |>
    
    mutate(
      aandeel=round(aantal_banen/sum(aantal_banen)*100)) |>
    
    mutate(sector = factor(sector, levels = c(
      "detailhandel dagelijks", 
      "detailhandel niet-dagelijks", 
      "horeca", "totaal")
    )
    )
  
}

cbs_data_df <- cbs_data |>
  map_df(\(x) my_perc(x))

cbs_tabel<- cbs_data_df |>
  ungroup()|>
  select(jaar, sector, indicator, categorieen, aandeel)



library(patchwork) 

my_plot<- function (x, var, var_levels) {
  
  x |>
    filter(indicator == var,
           jaar == 'banen_bedrijven_dec2022') |>
    
    mutate(categorieen = factor(categorieen, levels = var_levels))|>
    
    ggplot(aes(x= aandeel, y= fct_rev(sector), fill= fct_rev(categorieen)))+
    geom_col()+
    
    geom_text( 
      aes(label = if_else(aandeel > 10, glue::glue("{aandeel}%")," "),
          group = fct_rev(categorieen)),
      position = position_stack(vjust = 0.5),
      family = font
      )+
    
    labs(title=NULL, x=NULL, y = NULL) +
    theme_os()+ 
    theme(
      strip.text = element_text(size = 12, family = font))+
    guides(fill = guide_legend(reverse = T, ncol = 2))
  
}

a <- cbs_tabel |>
  my_plot ('opleiding', opl_levels)+
  scale_fill_manual(name= NULL, values = blauw_pal[c(9,1,3,5,7)])

b <- cbs_tabel |>
  my_plot ('etnische achtergrond',  etn_levels)+
  scale_fill_manual(name= NULL, values = blauw_pal[c(9,1,2,3,4, 5,6,7)])+
  theme(axis.text.y = element_blank())

def1<- a+b
ggsave("04 output tabellen/fig1_abrpolis_opl_etn.png", width = 11, height = 4)


c <- cbs_tabel |>
  my_plot ('leeftijd', lft_levels)+
  scale_fill_manual(name= NULL, values = blauw_pal[c(9,1,3,5,6,7,8)])

d <- cbs_tabel |>
  my_plot ('woonregio', woon_levels)+
  scale_fill_manual(name= NULL, values = blauw_pal[c(9,1,5)])+
  theme(axis.text.y = element_blank())

def2<- c+d
ggsave("04 output tabellen/fig1_abrpolis_lft_lft.png", width = 11, height = 4)

       

