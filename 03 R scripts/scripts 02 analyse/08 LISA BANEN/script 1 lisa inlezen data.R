library(tidyverse)
library(openxlsx)


#source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

source("03 R scripts/scripts 02 analyse/08 LISA BANEN/script 00 setup.R")

### uit de referentiedatabase zonder geo heel amsterdam ---

lisa_data12_22 <- bind_rows(
  
  # data Amsterdam totaal
  dbGetQuery(
    conn = os_db_con(
      db_name = 'refdb_az',path = pad_thuis_nieuw),
  statement = "
  SELECT peildatum, gemeentecode, gemeentenaam, detailhandel_code, detailhandel_omschrijving,
  COUNT (*) AS aantal_vestigingen, SUM (aantal_werkzamepersonen) AS aantal_banen
  FROM public.standbedrijven_vestigingen
  WHERE gemeentecode in ('0363', '0457') 
  AND aantal_werkzamepersonen > 0 
  GROUP BY peildatum, gemeentecode, gemeentenaam, detailhandel_code, detailhandel_omschrijving
  ;
  ")|>
  
    rename (sdl_code = gemeentecode, 
            sdl_naam = gemeentenaam),
  
  # data naar stadsdeel
  dbGetQuery(
    conn = os_db_con(
      db_name = 'refdb_az', path = pad_thuis_nieuw),
  statement = "
  SELECT peildatum, sdl_code, sdl_naam, detailhandel_code, detailhandel_omschrijving, 
  COUNT (*) AS aantal_vestigingen, SUM (aantal_werkzamepersonen) AS aantal_banen
  FROM public.standbedrijven_vestigingen
  WHERE gemeentecode in ('0363', '0457') 
  AND aantal_werkzamepersonen > 0 
  GROUP BY peildatum, sdl_code, sdl_naam, detailhandel_code, detailhandel_omschrijving
  ;
  ")
  
)


lisa_data_DEF <- lisa_data12_22 |>
  
  filter(
    !is.na(detailhandel_code),
    detailhandel_code != '11',   # ambulante handel
    detailhandel_code != '12',   # garagebedrijven
    detailhandel_code != '16')|> # webwinkels
  
  mutate(sector = case_when(
    detailhandel_code %in% c("01", "02", "14", "15") ~ "dagelijkse boodschappen",
    TRUE ~ "niet-dagelijkse producten")
  )


lisa_data_totaal <- lisa_data_DEF |>
  
  group_by(peildatum,sector ) |>
  
  summarise (aantal_vestigingen=sum(aantal_vestigingen),
             aantal_banen=sum(aantal_banen))|>
  pivot_wider(values_from = c(aantal_vestigingen, aantal_banen), names_from = sector)


write.xlsx(list(lisa_data_DEF,lisa_data_totaal), "04 output tabellen/tabel_lisa12_22.xlsx", overwrite = T)
                                                     
source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")


# winkels dagelijse boodschappen
lisa_data_DEF |>
  
  filter(sector == 'dagelijkse boodschappen') |>
  
  group_by(detailhandel_code, detailhandel_omschrijving,  peildatum, sector) |>
  
  summarise (aantal_vestigingen=sum(aantal_vestigingen),
             aantal_banen=sum(aantal_banen))|>
  
  ggplot(aes(x=peildatum,y=aantal_banen))+
  geom_col(fill = palettes_list$blauw[2])+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  theme_os() +
  theme(
    strip.text = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_fill_manual(values=palettes_list$blauw[c(2,5)])+
  facet_wrap(~ detailhandel_omschrijving, scales = "free_y")
ggsave("04 output tabellen/fig_lisa_DG.png", width = 8, height = 4)


# winkels niet-dagelijkse boodschappen
lisa_data_DEF |>
  
  filter(sector == 'niet-dagelijkse producten') |>
  
  group_by(detailhandel_code, detailhandel_omschrijving,  peildatum, sector) |>
  
  summarise (aantal_vestigingen=sum(aantal_vestigingen),
             aantal_banen=sum(aantal_banen))|>
  
  mutate(detailhandel_omschrijving=case_when(
    detailhandel_omschrijving == 'witgoed, bruingoed en geluidsdragers'  ~ 'electronica',
    detailhandel_omschrijving == 'ijzerwaren en gereedschappen'          ~ 'ijzerwaren',
    detailhandel_omschrijving == 'reparatie consumentengoederen'         ~ 'reparatie',
    TRUE ~ detailhandel_omschrijving)
  ) |>
  
  ggplot(aes(x=peildatum,y=aantal_banen))+
  geom_col(fill = palettes_list$blauw[2])+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  theme_os() +
  theme(
    strip.text = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_fill_manual(values=palettes_list$blauw[c(2,5)])+
  facet_wrap(~ fct_reorder(detailhandel_omschrijving, detailhandel_code), scales = "free_y")
ggsave("04 output tabellen/fig_lisa_NDG.png", width = 9, height = 5)


# banen per stadsdeel
lisa_data_DEF |>
  
  group_by(detailhandel_code, detailhandel_omschrijving,  peildatum, sector) |>
  
  summarise (aantal_vestigingen=sum(aantal_vestigingen),
             aantal_banen=sum(aantal_banen))|>
  
  mutate(detailhandel_omschrijving=case_when(
    detailhandel_omschrijving == 'witgoed, bruingoed en geluidsdragers'  ~ 'electronica',
    detailhandel_omschrijving == 'ijzerwaren en gereedschappen'          ~ 'ijzerwaren',
    detailhandel_omschrijving == 'reparatie consumentengoederen'         ~ 'reparatie',
    TRUE ~ detailhandel_omschrijving)
  ) |>
  
  ggplot(aes(x=peildatum,y=aantal_banen))+
  geom_col(fill = palettes_list$blauw[2])+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = \(x) scales::comma(x, decimal.mark = ",", big.mark = "."))+
  theme_os() +
  theme(
    strip.text = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))+
  scale_fill_manual(values=palettes_list$blauw[c(2,5)])+
  facet_wrap(~ fct_reorder(detailhandel_omschrijving, detailhandel_code), scales = "free_y")
ggsave("04 output tabellen/fig_lisa_NDG.png", width = 9, height = 5)

#### HORECA ---

lisa_horeca <- bind_rows(
  
  # data Amsterdam totaal
  dbGetQuery(
    conn = os_db_con(
      db_name = 'refdb_az',path = pad_thuis_nieuw),
    statement = "
  SELECT peildatum, gemeentecode, gemeentenaam, horeca_code,	horeca_omschrijving,
  COUNT (*) AS aantal_vestigingen, SUM (aantal_werkzamepersonen) AS aantal_banen
  FROM public.standbedrijven_vestigingen
  WHERE gemeentecode in ('0363', '0457') 
  AND aantal_werkzamepersonen > 0 
  GROUP BY peildatum, gemeentecode, gemeentenaam, horeca_code,	horeca_omschrijving
  ;
  ")|>
    
    rename (sdl_code = gemeentecode, 
            sdl_naam = gemeentenaam),
  
  # data naar stadsdeel
  dbGetQuery(
    conn = os_db_con(
      db_name = 'refdb_az', path = pad_thuis_nieuw),
    statement = "
  SELECT peildatum, sdl_code, sdl_naam, horeca_code,	horeca_omschrijving, 
  COUNT (*) AS aantal_vestigingen, SUM (aantal_werkzamepersonen) AS aantal_banen
  FROM public.standbedrijven_vestigingen
  WHERE gemeentecode in ('0363', '0457') 
  AND aantal_werkzamepersonen > 0 
  GROUP BY peildatum, sdl_code, sdl_naam, horeca_code,	horeca_omschrijving
  ;
  ")
  
)



write.xlsx(lisa_horeca, "04 output tabellen/tabel_hor12_22.xlsx", overwrite = T)


