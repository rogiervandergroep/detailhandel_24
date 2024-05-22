



library(tidyverse)
library(openxlsx)

tabel<- read_rds("04 output tabellen/tabel_locatus.rds")




source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

tabel$drie_gebied |>
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
tabel$drie_gebied  |>
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
tabel$drie_gebied  |>
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


tabel$drie_gebied  |>
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


tabel$drie_gebied  |>
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
tabel$drie_gebied  |>
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
tabel$drie_gebied  |>
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
tabel$drie_gebied  |>
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
tabel$drie_gebied  |>
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
tabel$drie_gebied  |>
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


list_dataverhaal <- read_rds("04 output tabellen/tabel_dataverhaal.rds")
