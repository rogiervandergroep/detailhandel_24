library(sf)
library(tidyverse)

#source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

my_spatial_type<- function(type, jaar, datum) {
  
   pad <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/amsterdam/"
  #pad <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/main/geo/"
   

  
    st_read(glue::glue("{pad}/{jaar}/{type}-{jaar}-geo.json")) |>
    st_drop_geometry()|> 
    add_column(spatial_date = datum,
               spatial_type = type)  |>
    rename(spatial_code = code, 
           spatial_name = naam) |>
    select(spatial_code, spatial_name, spatial_date, spatial_type)
  
}

geo_list<- bind_rows(
  
  # nieuwe gebiedsindeling
  tibble(spatial_code = c("0363"), spatial_name = "Amsterdam", spatial_type = 'gemeente', spatial_date ='20220324'), 
  
  my_spatial_type("buurten"   , 2022, '20220324'),
  my_spatial_type("wijken"    , 2022, '20220324'),
  my_spatial_type("stadsdelen", 2022, '20220324'),
  my_spatial_type("gebieden"  , 2022, '20220324'),
  
  # nieuwe winkelgebieden
  st_read(
    "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/winkelgebieden/2024/winkelgebieden-2024-geo.json?ref_type=heads") |>
    st_drop_geometry()|> 
    add_column(spatial_date = "20240101",
               spatial_type = "winkelgebieden")  |>
    rename(spatial_code = code, 
           spatial_name = naam) |>
    select(spatial_code, spatial_name, spatial_date, spatial_type),
  
  # oude gebiedsindeling
  tibble(spatial_code = c("AMS"), spatial_name = "Amsterdam", spatial_type = 'gemeente', spatial_date = '20150101'),
  
  my_spatial_type("buurten"   , "2015-2020", '20150101'),
  my_spatial_type("wijken"    , "2015-2020", '20150101'),
  my_spatial_type("stadsdelen", "2015-2020", '20150101'),
  my_spatial_type("gebieden"  , "2015-2020", '20150101')
)
  
  
  