
library(tidyverse)
library(openxlsx)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


tabel <- read.xlsx("00 ruwe data 2022 2023/tabel abr_polis detailhandel.xlsx") |>

  pivot_longer(cols= c(dagelijks:amsterdam))

opl_levels <- c('vmbo-geschoold', 'mbo-geschoold', 'hbo, wo-geschoold', 'opleiding onbekend')
lft_levels <- c('jonger dan 18 jaar', '18 - 34 jaar','35 - 55 jaar','55 - 67 jaar','68 jaar en ouder','leeftijd onbekend')
etn_levels <- c('autochtoon',
                'marokko',
                'turkije',
                'suriname en antillen',
                'ov. niet-westers',
                'ov. westers',
                'herkomst onbekend')
                



# aandeel dat geen markt bezoekt -
tabel |>
  filter(thema == 'opleiding') |>
  mutate(variabele = factor(
    variabele, levels = opl_levels)) |>
  ggplot(aes(x= value, y= name, fill= fct_rev(variabele) ))+
  geom_col()+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = wild_pal[c(2,3,4,5)]) +
  guides(fill = guide_legend(reverse = T))
ggsave("04 output tabellen/fig1_abrpolis_opl.png", width= 7, height = 4)

tabel |>
  filter(thema == 'leeftijd') |>
  mutate(variabele = factor(
    variabele, levels = lft_levels)) |>
  ggplot(aes(x= value, y= name, fill= fct_rev(variabele) ))+
  geom_col()+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = wild_pal[c(2,3,4,5,6, 7)]) +
  guides(fill = guide_legend(reverse = T))
ggsave("04 output tabellen/fig1_abrpolis_lft.png", width= 7, height = 4)


tabel |>
  filter(thema == 'herkomst') |>
  mutate(variabele = factor(
    variabele, levels = etn_levels)) |>
  ggplot(aes(x= value, y= name, fill= fct_relevel(fct_rev(variabele), "herkomst onbekend" )))+
  geom_col()+
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os()+ 
  scale_fill_manual(name= NULL, values = palettes_list$fruitig[c(9,8,7,6,5,4,3,2,1)]) +
  guides(fill = guide_legend(reverse = T))
ggsave("04 output tabellen/fig1_abrpolis_etn.png", width= 7, height = 4)


#"#a00078" "#e50082" "#009dec" "#fb9bbe" "#d48fb9" "#a4ccf3" "#ffd8e5" "#efd2e3" "#dceafa"
