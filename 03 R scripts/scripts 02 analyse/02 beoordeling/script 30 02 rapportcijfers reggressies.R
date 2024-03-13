
library(tidyverse)
library(openxlsx)

source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

df_rapportcijfers <- read.xlsx("04 output tabellen/tabel_rapportcijfers24_DG_NDG.xlsx") |>
  filter(
    winkelgebied_naam_kort != 'Amsterdam totaal',
    item != 'gemiddeld rapportcijfer',
    !is.na(winkelgebied_code),
    !grepl("Overig", winkelgebied_naam_kort),
    !grepl("overig", winkelgebied_naam_kort)) |>
  select(item, winkelgebied_naam_kort, gemiddelde, productgroep) |>
  pivot_wider(names_from = item, values_from = gemiddelde) 

df_rapcijfers_dg <- df_rapportcijfers |>
  filter(productgroep =='winkelgebied voor dagelijkse boodschappen')


df_rapcijfers_ndg <- df_rapportcijfers |>
  filter(productgroep =='winkelgebied voor niet-dagelijkse boodschappen')



model_dg <- lm(
  `totaaloordeel winkelgebied` ~ 
  `sfeer en de gezelligheid`+
  `het diverse aanbod van food-winkels` + 
  `het diverse aanbod van non-food-winkels` + 
  `het biologische/duurzame aanbod food` + 
  `schoonhouden van de straten` ,
  
  data = df_rapcijfers_dg)

summary(model_dg)


data_model <- df_rapcijfers_dg|>
  
  select(
    winkelgebied_naam_kort,
    `totaaloordeel winkelgebied`,
    `sfeer en de gezelligheid`,
    `het diverse aanbod van food-winkels` , 
    `het biologische/duurzame aanbod food`, 
    `schoonhouden van de straten`) |>
  
  pivot_longer(cols = c(`sfeer en de gezelligheid`: `schoonhouden van de straten`)) |>
  
  mutate(name = factor(name, levels= c(
    "het diverse aanbod van food-winkels",
    "sfeer en de gezelligheid",
    "schoonhouden van de straten",
    "het biologische/duurzame aanbod food")))


plot <- data_model |> 
  
  ggplot(aes(
    label = winkelgebied_naam_kort,
    x= value,  
    y= `totaaloordeel winkelgebied`))+
  ggrepel::geom_text_repel()+
  geom_point()+
  geom_smooth(method='lm')+
  theme_os()+
  xlab(NULL)+
  facet_wrap(~name)

ggsave("04 output tabellen/fig_rap_regressie.png", width = 11, height = 7)



