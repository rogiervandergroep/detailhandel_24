

# lees libraries in met setup -
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# inlezen data 2024 met weegfactoren -  
load(file = "03 tussentijds/data_24_DEF.RDS")

data_24_basis <- data_24_weeg |>
  mutate(respdef = 
           str_glue("data24_{Tranche}_{row_number()}")
  )

# inlezen voorgaande jaargangen - 

load("03 tussentijds/datadef22.RData")

list_18_22 <- list(
  
  data_18_basis =  datadef18, 
  data_20_basis =  datadef20, 
  data_22_basis =  datadef21)

rm(datadef18, datadef20, datadef21)


# toevoegen nieuwe gebiedsindeling aan oude bestanden -

pc_data <- read.csv2("03 tussentijds/postcode6 naar alle indelingen 2022 alternatief aangepast.csv") |>
  select(postcode:gbd_sdl_naam)

# hercoderen achtergrondvars -
# inlezen functie - 

source("03 R scripts/scripts 02 analyse/00 01 script achtergrondvars.R")


herkomst_var <- c("OPLEID", "opleid", "OPLEID_panel", 
                  "INKOMEN", "inkomen", "INKOMEN_panel", 
                  "HHSAM", "hhsam", "HHSAM_panel", 
                  "GESL", "gesl", "LEEFTD", "leeftd")

herkomst_var_nieuw <- c("opleiding_klas",  "inkomen_klas", "huishouden_klas", "geslacht", "leeftijd_klas")


### selectie van data ---

data_herk<- list()

data_herk$data_24 <- data_24_basis |>
  select(respdef, any_of(herkomst_var))|>
  haven::as_factor()

data_herk$data_22 <- list_18_22$data_22_basis |>
  select(respdef, any_of(herkomst_var))|>
  haven::as_factor()

data_herk$data_20 <- list_18_22$data_20_basis |>
  select(respdef, any_of(herkomst_var))|>
  haven::as_factor()

data_herk$data_18 <- list_18_22$data_18_basis |>
  select(respdef, any_of(herkomst_var))|>
  haven::as_factor()



# hercoderen data
data_herk$data2_24 <- data_herk$data_24|>
  my_herkomst24()|>
  select (respdef, any_of(herkomst_var_nieuw))

data_herk$data2_22 <- data_herk$data_22 |>
  my_herkomst22()|>
  select (respdef, any_of(herkomst_var_nieuw))

data_herk$data2_20 <- data_herk$data_20 |>
  my_herkomst22()|>
  select (respdef, any_of(herkomst_var_nieuw))

# samenvoegen met originele data --

data_def<- list()

data_def$data_24_def<- data_24_basis|>
  left_join(data_herk$data2_24, by = "respdef") 

data_def$data_22_def<- list_18_22$data_22_basis|>
  left_join(data_herk$data2_22, by = "respdef")|>
  mutate(postcode_def=case_when(is.na(postcode_def) ~ pc,
                                TRUE ~postcode_def))|>
  mutate(postcode_def = str_trim(str_to_upper(postcode_def)), "both")|>
  left_join(pc_data , by = c("postcode_def"= "postcode"))

data_def$data_20_def<- list_18_22$data_20_basis|>
  left_join(data_herk$data2_20, by = "respdef")|>
  left_join(pc_data , by = c("pttkod"= "postcode"))

data_def$data_20_def[["weeg_ams"]]<- data_def$data_20_def[["wstad"]]
data_def$data_22_def[["weeg_ams"]]<- data_def$data_22_def[["wg"]]

save(data_def, pdg, df_omzetcijfers, file= "03 tussentijds/data_24_DEF.Rdata")

