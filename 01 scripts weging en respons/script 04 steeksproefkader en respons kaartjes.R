
source("01 scripts weging en respons/script 00 setup.R")

steekproefkader <- readRDS("03 output tabellen/steekproefkader.rds")

### KAART 1 TARGET PER GEBIED  ---


geo_json1 <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/main/geo/amsterdam/2022/gebieden-2022-geo.json"

kaart_25geb  <- geojson_sf(geo_json1) |>
  mutate(target= case_when(
    code %in% c('GF06' , 'GT21' ) ~ '50 respondenten',
    TRUE                          ~ '200 respondenten'))

kleuren<- c("#004699", "#5d6fb3", "#959dcc", "#cacde6")
  

plot_25geb <-  kaart_25geb |>
  ggplot()+
  geom_sf(aes(fill = target ), linewidth = 0.9 , color = "white" )+
  scale_fill_manual(values=kleuren[c(3,4)])+
  guides(fill=guide_legend(title=NULL))+
  theme_os_map()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

plot_25geb



### KAART 2 TARGET PER WIJK ---

geo_json2 <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/main/geo/amsterdam/2022/wijken-2022-geo.json"
  
kaart_wijken <- geojson_sf(geo_json2)|>
  left_join(steekproefkader, by=c("code"= "wijk_code") ) |>
  mutate(target_totaal=round(target_totaal)) |>
  filter(stadsdeelCode != "B")
  
plot_wijk <- kaart_wijken |>
  ggplot()+
  geom_sf(aes(), fill= kleuren[3], linewidth =0.8, colour='white')+
  geom_sf_text(aes(label = target_totaal),  fontsize = 1.8)+
  guides(fill = guide_legend(title=NULL))+
  theme_os_map()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank(),
        rect = element_blank())

plot_wijk

save(plot_25geb, plot_wijk, file="03 output tabellen/plots.Rdata")


