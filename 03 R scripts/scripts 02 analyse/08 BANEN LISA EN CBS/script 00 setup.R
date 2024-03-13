

library(tidyverse)
library(openxlsx)
library(sf)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

# pad_werk        = "H:/db_configs/referentiedb.yml"
# pad_thuis_oud   = "C:/Users/groep001/OneDrive - Gemeente Amsterdam/Documenten/referentiedb.yml"
pad_thuis_nieuw = "C:/Users/groep001/OneDrive - Gemeente Amsterdam/Documenten/db_configs.yml"

# handig voor een check in dbeaver
db_token <- get_azure_access_token()


my_spatial_type<- function(type, jaar, datum) {
  
    pad <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/amsterdam/"
    #pad <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/main/geo/"
    
    sf::st_read(glue::glue("{pad}/{jaar}/{type}-{jaar}-geo.json")) |>
      sf::st_drop_geometry()|> 
      add_column(spatial_date = datum,
                 spatial_type = type)  |>
      rename(spatial_code = code, 
             spatial_name = naam) |>
      select(spatial_code, spatial_name, spatial_date, spatial_type)
    
}


# indeling 2022
geo_list<- list()
geo_list$stad_22       <- tibble(spatial_code = c("0363"), spatial_name = "Amsterdam", spatial_type = 'gemeente', spatial_date ='20220324')
geo_list$buurten_22    <- my_spatial_type("buurten"   , 2022, '20220324')
geo_list$wijken_22     <- my_spatial_type("wijken"    , 2022, '20220324')
geo_list$stadsdelen_22 <- my_spatial_type("stadsdelen", 2022, '20220324')
geo_list$gebieden_22   <- my_spatial_type("gebieden"  , 2022, '20220324') 

# indeling 2015
geo_list$stad_15       <- tibble(spatial_code = c("AMS"), spatial_name = "Amsterdam", spatial_type = 'gemeente') |> add_column(spatial_date = '20150101')
geo_list$buurten_15    <- my_spatial_type("buurten"   , '2015-2020', '20150101')
geo_list$wijken_15     <- my_spatial_type("wijken"    , '2015-2020', '20150101')
geo_list$stadsdelen_15 <- my_spatial_type("stadsdelen", '2015-2020', '20150101')
geo_list$gebieden_15   <- my_spatial_type("gebieden"  , '2015-2020', '20150101') 


geo_list_df_22 <- geo_list |>
  map(\(x) filter(x, spatial_date =='20220324')) |>
  bind_rows()

geo_list_df_15 <- geo_list |>
  map(\(x) filter(x, spatial_date =='20150101'))|>
  bind_rows()
      
      
      

# wg_24<- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/geo/winkelgebieden/2024/winkelgebieden-2024-geo.json"
# geo_list$winkelgebieden <- jsonlite::fromJSON(wg_24)[["features"]][["properties"]][, c("code", "naam")]

