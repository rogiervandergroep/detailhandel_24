library(tidyverse)
library(openxlsx)
library(glue)

pad<- "00 ruwe data 2022 2023/data locatus/"

locatus <-list(
  
  read.csv2(glue("{pad}VKP24.csv"))|>add_column(jaar= '2024'),
  read.csv (glue("{pad}VKP23.csv"))|>add_column(jaar= '2023'),
  read.csv2(glue("{pad}VKP22.csv"))|>add_column(jaar= '2022'),
  read.csv2(glue("{pad}VKP21.csv"))|>add_column(jaar= '2021'),
  read.csv (glue("{pad}VKP20.csv"))|>add_column(jaar= '2020'),
  read.csv (glue("{pad}VKP19.csv"))|>add_column(jaar= '2019'),
  read.csv (glue("{pad}VKP18.csv"))|>add_column(jaar= '2018'),
  read.csv (glue("{pad}VKP17.csv"))|>add_column(jaar= '2017'),
  read.csv (glue("{pad}VKP16.csv"))|>add_column(jaar= '2016'),
  read.csv (glue("{pad}VKP15.csv"))|>add_column(jaar= '2015'),
  read.csv (glue("{pad}VKP14.csv"))|>add_column(jaar= '2014')
)

postcode <- read.csv2("02 lookup tabellen/postcode6 naar alle indelingen 2022 alternatief aangepast.csv")


my_pc_join <-function(x){
  
  x|>
    left_join(
      postcode, by=c("POSTCODE"="postcode" )
    )
}

locatus_list_pc <- map(locatus, my_pc_join)

## toeven  branches; drie gebieden

detailhandel_dagelijks <- c("11-Dagelijks")
                  
detailhandel_nietdagelijks <- c(
  "22-Mode & Luxe",
  "35-Vrije Tijd",
  "37-In/Om Huis",
  "38-Detailh Overig")

horeca23 <- c("59-Horeca")
horeca18 <- c("59.210-Horeca")

leisure23 <- c("58-Cultuur & Ontspanning", "65-Diensten")
leisure18 <- c("59.220-Cultuur","59.230-Ontspanning")


my_sectoren <- function(x){
  

x |>
  
  mutate(sectoren=
           case_when(
             GROEP == "00-Leegstand" ~ 'leegstand',
             GROEP %in% detailhandel_dagelijks ~ 'detailhandel dagelijks',
             GROEP %in% detailhandel_nietdagelijks ~ 'detailhandel niet-dagelijks',
             GROEP %in% horeca23 ~ 'horeca',
             HOOFDBRANCHE %in% horeca18 ~ 'horeca',
             GROEP %in% leisure23 ~ 'ontspanning en diensten',
             HOOFDBRANCHE %in% leisure18 ~ 'ontspanning en diensten',
             TRUE ~ 'overig')
         )|>
  
  mutate(drie_gebieden =
           case_when(
             
             gbd_sdl_naam == 'Centrum' ~ 'Centrum',
             gbd_sdl_naam %in% c('Zuid', 'Oost', 'West') ~ 'West, Zuid en Oost',
             TRUE ~ 'NW, Noord, ZO, Weesp')
         )

}

loc_list_pc <- map(locatus_list_pc, my_sectoren)

### verwijderen servicepunten en atm (alleen in oude jaargangen)
my_filter<- function(x){
  
  x |>
    filter(
      
      NAAM  != 'SERVICEPUNT',
      GROEP != '80-ATM'
      )
}

loc_list_pc <- loc_list_pc |>
  map(\(x) my_filter(x))

my_summary <-function(x){
  
  bind_rows(
    
    x|>
      group_by(
        gbd_sdl_code, gbd_sdl_naam, drie_gebieden,  sectoren, jaar) |>
      
      summarise(
        aantal=n(),
        oppervlakte=sum(WVO, na.rm = T)
      ),
    
    x|>
      group_by(
        sectoren, jaar) |>
      
      summarise(
        aantal=n(),
        oppervlakte=sum(WVO, na.rm = T)
      )|>
      
      add_column(
        gbd_sdl_code  = 'Amsterdam', 
        gbd_sdl_naam  = 'Amsterdam', 
        drie_gebieden = 'Amsterdam')
    )
  

  
}

list_df <- map_df(loc_list_pc, my_summary)

# om synchroon te lopen met locatus Nederland wordt tot en met 2019 2/3 van de leegstand genomen

list_df <- list_df |>
  mutate(
    oppervlakte=case_when(
      (sectoren == 'leegstand' & jaar %in%
         c("2014", "2015", "2016", "2017", "2018", "2019")) ~ round(oppervlakte*2/3),
      TRUE ~ oppervlakte)
  )

### samenvatting

my_table<- function(x, group_var){
  
 x |>
    
    group_by(
      across({{group_var}}), sectoren, jaar)|>
    
    summarise(
      `aantal vestigingen`=sum(aantal),
      `oppervlakte (m2)`=sum(oppervlakte)) |>
    
    group_by(
      across({{group_var}}), sectoren)|>
    
    mutate(
      `ontw. vest.`=`aantal vestigingen`/first(`aantal vestigingen`)*100,
      `ontw. oppervl.`=`oppervlakte (m2)`/first(`oppervlakte (m2)`)*100)  |>
    
    group_by(
      across({{group_var}}), jaar) |>
    
    mutate(
      `aandeel vestigingen (%)` = `aantal vestigingen`/sum(`aantal vestigingen`, na.rm = T)*100,
      `aandeel oppervlakte (%)` = `oppervlakte (m2)`/sum(`oppervlakte (m2)`, na.rm = T)*100) |>
    
    pivot_longer(
      where(is.numeric))
  
}

sd_levels <- c("Centrum", "Westpoort", "West", "Nieuw-West", "Zuid", "Oost", "Noord", "Weesp", "Zuidoost", "Amsterdam")

tabel_drie_sec<- list_df |>
  my_table(gbd_sdl_naam)
  

tabel_drie_geb_sec<- list_df |>
  my_table(drie_gebieden)

tabel_drie_sec_wide <- tabel_drie_sec |>
  pivot_wider(names_from = jaar, values_from = value)

tabel_drie_geb_sec_wide <- tabel_drie_geb_sec |>
  pivot_wider(names_from = jaar, values_from = value)

write.xlsx(list(tabel_drie_sec_wide,tabel_drie_geb_sec_wide ), "04 output tabellen/tabel_locatus.xlsx", withFilter=T, overwrite = T)


source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("aantal vestigingen", "oppervlakte (m2)"),
    sectoren %in% c("detailhandel dagelijks", "detailhandel niet-dagelijks"),
    gbd_sdl_naam == "Amsterdam")|>
  
  ggplot(aes(x=jaar,y=value, group= sectoren))+
  geom_line(aes(color= sectoren), size = 1.2)+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  expand_limits(y = 0)+
  theme_os() +
  theme(
    strip.text = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~name, scales = "free_y")
ggsave("04 output tabellen/fig_loc_01.png", width = 8, height = 3)

# leegstand
tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("aantal vestigingen", "oppervlakte (m2)"),
    sectoren %in% c("leegstand"),
    gbd_sdl_naam == "Amsterdam")|>
  
  ggplot(aes(x=jaar,y=value, group= sectoren))+
  geom_line(aes(color= sectoren), size = 1.2)+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  expand_limits(y = 0)+
  theme_os() +
  theme(
    legend.position = 'none',
    strip.text = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~name, scales = "free_y")
ggsave("04 output tabellen/fig_loc_leegstand.png", width = 8, height = 3)

# leegstand aandeel
tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("aandeel vestigingen (%)", "aandeel oppervlakte (%)"),
    sectoren %in% c("leegstand"),
    gbd_sdl_naam == "Amsterdam")|>
  
  ggplot(aes(x=jaar,y=value, group= sectoren))+
  geom_line(aes(color= sectoren), size = 1.2)+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  expand_limits(y = 0)+
  theme_os() +
  theme(
    legend.position = 'none',
    strip.text = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~name)
ggsave("04 output tabellen/fig_loc_leegstand_p.png", width = 8, height = 3)


tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("aantal vestigingen"),
    sectoren %in% c("detailhandel dagelijks", "detailhandel niet-dagelijks", "leegstand"),
    gbd_sdl_naam != "Amsterdam")|>
  ggplot(aes(x=jaar,y=value, fill= sectoren))+
  geom_col()+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  theme_os() +
  theme(
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_fill_manual(values=palettes_list$wild)+
  facet_wrap(~ gbd_sdl_naam, scales = "free_y")
ggsave("04 output tabellen/fig_loc_02.png", width = 8, height = 5)


tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("oppervlakte (m2)"),
    sectoren %in% c("detailhandel dagelijks", "detailhandel niet-dagelijks", "leegstand"),
    gbd_sdl_naam != "Amsterdam")|>
  ggplot(aes(x=jaar,y=value, fill= sectoren))+
  geom_col()+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  theme_os() +
  theme(
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_fill_manual(values=palettes_list$wild)+
  facet_wrap(~ gbd_sdl_naam, scales = "free_y")
ggsave("04 output tabellen/fig_loc_03.png", width = 8, height = 5)


#leegstand per sd
tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("aandeel vestigingen (%)", "aandeel oppervlakte (%)"),
    sectoren %in% c("leegstand"),
    !is.na(gbd_sdl_naam))|>
  
  ggplot(aes(x=jaar,y=value, group= name))+
  geom_line(aes(color= name), size = 1.2)+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(breaks=seq(0, 16, 4), labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  theme_os() +
  expand_limits(y = 0)+
  theme(
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~ gbd_sdl_naam)
ggsave("04 output tabellen/fig_loc_leegstand_sd.png", width = 8, height = 5)


# ontwikkeling vestigingen 
tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("ontw. vest."),
    sectoren %in% c("detailhandel dagelijks", "detailhandel niet-dagelijks"),
    gbd_sdl_naam != "Westpoort")|>
  ggplot(aes(x=jaar,y=value, group= sectoren))+
  geom_line(aes(color=sectoren))+
  geom_point(aes(color=sectoren))+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  theme_os() +
  theme(
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~ gbd_sdl_naam, scales = "free_y")
ggsave("04 output tabellen/fig_loc_04_ves.png", width = 8, height = 5)

# ontwikkeling oppervlakte
tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("ontw. oppervl."),
    sectoren %in% c("detailhandel dagelijks", "detailhandel niet-dagelijks"),
    gbd_sdl_naam != "Westpoort")|>
  ggplot(aes(x=jaar,y=value, group= sectoren))+
  geom_line(aes(color=sectoren))+
  geom_point(aes(color=sectoren))+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  theme_os() +
  theme(
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~ gbd_sdl_naam, scales = "free_y")
ggsave("04 output tabellen/fig_loc_04_opp.png", width = 8, height = 5)


# ontwikkeling leegstand
tabel_drie_sec |>
  filter(
    jaar != 2013,
    name %in% c("ontw. oppervl.", "ontw. vest."),
    sectoren %in% c("leegstand"),
    gbd_sdl_naam != "Westpoort")|>
  ggplot(aes(x=jaar,y=value, group= name))+
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  theme_os() +
  theme(
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~ gbd_sdl_naam, scales = "free_y")
ggsave("04 output tabellen/fig_loc_05_leegstand.png", width = 8, height = 5)


# ontwikkeling horeca
tabel_drie_sec |>
  filter(
    jaar > 2013,
    name %in% c("aantal vestigingen"),
    sectoren %in% c("horeca", "ontspanning en diensten"),
    gbd_sdl_naam == "Amsterdam")|>
  ggplot(aes(x=jaar,y=value, group = sectoren))+
  geom_line(aes(color=sectoren), size = 0.9)+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  scale_x_discrete(breaks=seq(2014, 2024, 2))+
  expand_limits(y = 0)+
  theme_os() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_color_manual(values=palettes_list$wild)+
  facet_wrap(~ sectoren)
  
ggsave("04 output tabellen/fig_loc_06_horeca.png", width = 8, height = 3)
