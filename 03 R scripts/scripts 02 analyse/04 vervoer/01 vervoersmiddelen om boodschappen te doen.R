
# V5 hoe gaat u normaliter boodschappen doen?


### lees libraries in met setup ---
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

### opstarten ---
load("03 tussentijds/data_24_DEF.Rdata")


achtergrondvar<- c(
  "respdef",
  "opleiding_klas", "inkomen_klas", 
  "huishouden_klas", "geslacht", 
  "leeftijd_klas", 
  "gbd_brt_code", "gbd_brt_naam",
  "gbd_wijk_code","gbd_wijk_naam",
  "gbd_ggw_code", "gbd_ggw_naam",
  "gbd_sdl_code", "gbd_sdl_naam")

jaren<- c("monitor 2024", "monitor 2022", "monitor 2020")

data_v5<- data_def |>
  map(\(x) janitor::clean_names(x)) |>
  map(\(x) as_factor(x)) |>
  map(\(x) select(x, v5, v5_anders, all_of(achtergrondvar), weeg_ams)) |>
  map2(jaren, \(x,y) add_column(x, monitor= y)) |>
  bind_rows() |>
  filter(!is.na(v5)) |>
  mutate(
    v5=str_trim(v5, "both"),
    v5=case_when(
      v5 == 'bromfiets'~'brommer',
      v5 == 'brommer, bromfiets, scooter'~'brommer',
      v5 == 'anders, namelijk' ~ 'anders',
      TRUE ~ v5))

tabel_v5 <- data_v5 |>
  group_by(v5, monitor) |>
  summarise(aantal     = n(),
            aantal_gew = sum(weeg_ams, na.rm=T))|>
  group_by(monitor) |>
  mutate(aandeel     = round(aantal / sum (aantal, na.rm=T) * 100, 2),
         aandeel_gew = round(aantal_gew / sum (aantal_gew, na.rm=T)*100, 2))


sd_levels<- c(
  "Centrum",   "Westpoort",   "West",   "Nieuw-West", 
  "Zuid", "Oost", "Noord", "Weesp", "Zuidoost", 
  "Amsterdam totaal", "Amsterdam")

my_v5_function <- function(x, var, var_levels) {
  
  x |>
    group_by(monitor, onderwerp = {{var}}, v5) |>
    summarise(aantal     = n(),
              aantal_gew = sum(weeg_ams, na.rm=T))|>
    group_by(monitor, onderwerp) |>
    mutate(aandeel     = round(aantal / sum (aantal, na.rm=T) * 100, 2),
           aandeel_gew = round(aantal_gew / sum (aantal_gew, na.rm=T)*100, 2))
      }

tabel_v5_achtergr <- bind_rows(
  
  data_v5 |>
    my_v5_function (gbd_sdl_naam)|>
    mutate(onderwerp = factor(onderwerp, levels = sd_levels)) |>
    add_column(thema= "stadsdeel"),
  
  data_v5 |>
    my_v5_function (huishouden_klas)|>
    add_column(thema= "huishoudtype"),
  
  data_v5 |>
    my_v5_function (leeftijd_klas)|>
    add_column(thema= "leeftijdsklasse")
)

tabel_v5|>
  
  ggplot(aes(
    y = fct_rev(monitor),
    fill = fct_relevel(fct_reorder(v5, aandeel_gew), "anders", "weet niet, geen antwoord"),
    x = aandeel_gew)
  )+
  
  geom_col()+
  geom_text(aes(label = if_else(aandeel_gew > 10,as.character(round(aandeel_gew)),"")), 
            position = position_stack(vjust =0.5),
            family=font, lineheight=.8)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os3(legend_position = "bottom")+ 
  scale_fill_manual(name= NULL, values = rev(palettes_list$fruitig))  +
  guides(fill = guide_legend(reverse = T)) 
ggsave("04 output tabellen/figv5_mobiliteit.png", width = 7, height = 3)  


tabel_v5_achtergr|>
  
  filter(
    thema != 'stadsdeel',
    monitor == 'monitor 2024',
    onderwerp != 'overig, of huishoudtype onbekend',
    onderwerp != 'onbekend',
    !is.na(onderwerp))|>
  
  ggplot(aes(
    y = fct_rev(onderwerp),
    fill = fct_relevel(fct_reorder(v5, aandeel_gew), "anders", "weet niet, geen antwoord"),
    x = aandeel_gew)
  )+
  geom_col()+
  geom_text(aes(label = if_else(aandeel_gew > 10,as.character(round(aandeel_gew)),"")), 
            position = position_stack(vjust =0.5),
            family=font, lineheight=.8)+
  
  labs(title=NULL, x=NULL, y = NULL) +
  theme_os3(legend_position = "bottom")+ 
  scale_fill_manual(name= NULL, values = rev(palettes_list$fruitig))  +
  guides(fill = guide_legend(reverse = T))+
  facet_wrap(~ thema , scales= "free")
ggsave("04 output tabellen/figv5_mobiliteit_sd.png", width = 8, height = 3)  

