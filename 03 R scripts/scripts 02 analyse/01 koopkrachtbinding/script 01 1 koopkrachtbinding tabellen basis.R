
library(tidyverse)
library(openxlsx)

### inlezen werkbetand 

load(file= "03 tussentijds/data_24_DEF.Rdata") # uit script 00 inlezen opschonen

# selectie voor koopkrachtbinding rechte tellingen 

sel_basis <- c("Respondent_ID", "weeg_ams", "weeg_ams_ink",
               "gbd_sdl_code", "gbd_sdl_naam", "weeg_sd", "weeg_sd_ink",
               "gbd_ggw_code", "gbd_ggw_naam", "weeg_geb", "weeg_geb_ink")

# omzetten data naar long format en toevoegen omzetcijfers         
data24<-list()

my_pivot_1 <- function (x, pcol){
  
  x |> 
    pivot_longer(
      cols=({{pcol}}), 
      names_to  = "productgroep_code", 
      values_to = "winkelgebied_code") |>
    
    mutate(
      winkelgebied_naam  = haven::as_factor(winkelgebied_code),
      winkelgebied_code  = str_pad(as.character(winkelgebied_code), 
                                   pad = "0", 
                                   side="left", 
                                   width = 3)) |>
    filter(
      !is.na(winkelgebied_code),
      gbd_sdl_code!='')
    
    
  
}

my_pivot_2ndg <- function (x, pcol){
  
  x |>
  
  pivot_longer(
    cols=({{pcol}}), 
    names_to = "productgroep_code", 
    values_to = "winkelgebied_code") |>
    
    mutate(
      winkelgebied_naam  = haven::as_factor(winkelgebied_code),
      winkelgebied_naam  = case_when(
      winkelgebied_naam == 'per postorder, catalogus' ~ 'internet, online',
      TRUE ~ winkelgebied_naam),
      
      winkelgebied_code  = case_when(
      winkelgebied_code==2 ~ "800",
      winkelgebied_code==3 ~ "800",
      winkelgebied_code==4 ~ "999"),
      productgroep_code=str_remove(productgroep_code, "_Codes")) |>
    
    filter(
      !is.na(winkelgebied_code),
      gbd_sdl_code!='')
  
}
my_pivot_2dg  <- function (x, pcol){
  
  x |>
    
    pivot_longer(
      cols=({{pcol}}), 
      names_to = "productgroep_code", 
      values_to = "winkelgebied_code") |>
    
    mutate(
      winkelgebied_naam  = haven::as_factor(winkelgebied_code),
      winkelgebied_naam  = case_when(
        winkelgebied_naam == 'per postorder, catalogus' ~ 'internet, online',
        TRUE ~ winkelgebied_naam),
      
      winkelgebied_code  = case_when(
        winkelgebied_code==2 ~ "800",
        winkelgebied_code==3 ~ "999"),
      productgroep_code=str_remove(productgroep_code, "_Codes")) |>
    
    filter(
      !is.na(winkelgebied_code),
      gbd_sdl_code!='')
  
}

df_omzetcijfers <- df_omzetcijfers |>
  set_names(c("pdg_code","pdg_naam","pdg_omzetcijfers"))

data24$ndg <- bind_rows( 
  
  # in winkelgebieden
  data_def$data_24_def |>  
    select (any_of(c(sel_basis, pdg$code)))|>
    my_pivot_1(pcol=any_of(pdg$code)),
  
  # overig
  data_def$data_24_def  |>
    select (any_of(c(sel_basis, pdg$anders)))|>
    my_pivot_2ndg(pcol=any_of(pdg$anders))) |>
  
  left_join(df_omzetcijfers, by= c("productgroep_code"="pdg_code"))
         

data24$dg <- bind_rows( 
  
  data_def$data_24_def |>
    select (any_of(c(sel_basis, "V1_nw")))|>
    my_pivot_1(pcol= any_of("V1_nw")),

  
  data_def$data_24_def |>
    select (any_of(c(sel_basis, "V1_nw_Codes")))|>
    my_pivot_2dg(pcol= any_of("V1_nw_Codes")))  |>
  
  add_column(pdg_omzetcijfers= 1,
             pdg_naam = 'dagelijkse producten')

# samenvoegen dagelijks en niet dagelijkse prodctgroepen en weghalen nvt 

data24$totaal <- bind_rows(data24$dg, data24$ndg) |>
  filter (winkelgebied_code != 999)
  
freq_kkb_24 <- list()

my_summary_1 <- function(x, weeg_inw, weeg_ink) {
  
  x |>
    
    summarise(
      aantal            =  n(),
      aantal_gw         =  sum( {{weeg_inw}}, na.rm=T),
      aantal_gw_omz     =  sum(({{weeg_inw}}*pdg_omzetcijfers), na.rm = T),
      aantal_gw_ink     =  sum( {{weeg_ink}}, na.rm=T),
      aantal_gw_omz_ink =  sum(({{weeg_ink}}*pdg_omzetcijfers), na.rm = T),
      )
    
    
  
}





# som per stadsdeel
freq_kkb_24$SD <-  data24$totaal  |>
  group_by( 
    gbd_sdl_code, gbd_sdl_naam, 
    pdg_naam, winkelgebied_code, winkelgebied_naam) |>
  my_summary_1(weeg_inw = weeg_sd, 
               weeg_ink = weeg_sd_ink) 



# som per gebied
freq_kkb_24$GEB <-  data24$totaal  |>
  group_by( 
    gbd_ggw_code, gbd_ggw_naam, 
    pdg_naam, winkelgebied_code, winkelgebied_naam) |>
  my_summary_1(weeg_inw = weeg_geb, 
               weeg_ink = weeg_geb_ink) 

# som totaal amsterdam
freq_kkb_24$AMS <-  data24$totaal  |>
  group_by( 
    pdg_naam, winkelgebied_code, winkelgebied_naam) |>
  my_summary_1(weeg_inw = weeg_ams, 
               weeg_ink = weeg_ams_ink)

# maak een basis lookuptable van winkelgebieden 

write.xlsx(freq_kkb_24$AMS, "02 lookup tabellen/winkelgebieden24_basis.xlsx")


# inlezen defintieve lookup tabel winkelgebieden met afzet_wijk, gebied en sd
winkelgebieden24_def <- read.xlsx("02 lookup tabellen/winkelgebieden24.xlsx") |>
  mutate(afzet_wijk_code= case_when(is.na(afzet_wijk_code) ~ "NA",
                                    TRUE ~ afzet_wijk_code))

# koppelen afzetgebieden aan frequentietabellen 
freq_kkb_24$AMS <- freq_kkb_24$AMS |>
  left_join(winkelgebieden24_def, by= c("winkelgebied_code", "winkelgebied_naam"))

freq_kkb_24$GEB <- freq_kkb_24$GEB |>
  left_join(winkelgebieden24_def, by= c("winkelgebied_code", "winkelgebied_naam"))

freq_kkb_24$SD <- freq_kkb_24$SD |>
  left_join(winkelgebieden24_def, by= c("winkelgebied_code", "winkelgebied_naam"))


#### #### ##### #### 
### frequenties met procenten ---
my_summary <- function(x){
  
  x |>   
    
    summarise(
      aantal        = sum(aantal),
      aantal_gw     = sum(aantal_gw),
      aantal_gw_omz = sum(aantal_gw_omz),
      aantal_gw_ink = sum(aantal_gw_ink),
      aantal_gw_omz_ink = sum(aantal_gw_omz_ink)
      )
    
}

my_mutate <- function(x){
  
  x |>
    
    mutate(aandeel            = round((aantal / sum(aantal))*100,2),
           aandeel_gw         = round((aantal_gw / sum (aantal_gw))*100,2),
           aandeel_gw_ink     = round((aantal_gw_ink / sum (aantal_gw_ink))*100,2),
           aandeel_gw_omz     = round((aantal_gw_omz/sum (aantal_gw_omz))*100,2),
           aandeel_gw_omz_ink = round((aantal_gw_omz_ink/sum (aantal_gw_omz_ink))*100,2),
    )
}

# frequenties totaal amsterdam naar afzet_stadsdeel
freq_kkb_24$AMS_sd <- bind_rows(
  
  freq_kkb_24$AMS |>
    group_by(pdg_naam, afzet_stadsdeel_code  ) |>
    my_summary() |>
    group_by(pdg_naam) |>
    my_mutate(),
  
  freq_kkb_24$AMS |>
    filter(pdg_naam != 'dagelijkse producten') |>
    group_by(afzet_stadsdeel_code  ) |>
    my_summary() |>
    my_mutate() |>
    add_column(pdg_naam = 'NDG_totaal')
)
  




# frequenties totaal amsterdam naar afzet_gebied
freq_kkb_24$AMS_geb <- bind_rows(
  
  # per pdg_naam
  freq_kkb_24$AMS |>
    group_by(pdg_naam, afzet_gebied_code  ) |>
    my_summary() |>
    group_by(pdg_naam) |>
    my_mutate(),
  
  freq_kkb_24$AMS |>
    filter(pdg_naam != 'dagelijkse producten') |>
    group_by(afzet_gebied_code) |>
    my_summary()|>
    my_mutate()|>
    add_column(pdg_naam = 'NDG_totaal')
) 
  
  
  

# frequenties van herkomstgebied naar afzetgebied
freq_kkb_24$GEB_geb <- bind_rows (
  
  freq_kkb_24$GEB |>
    group_by(pdg_naam, gbd_ggw_code, gbd_ggw_naam, afzet_gebied_code  ) |>
    my_summary()|>
    group_by(pdg_naam, gbd_ggw_code, gbd_ggw_naam) |>
    my_mutate(),
  
 freq_kkb_24$GEB |>
    filter(pdg_naam != 'dagelijkse producten') |>
    group_by( gbd_ggw_code, gbd_ggw_naam, afzet_gebied_code  ) |>
    my_summary()|>
    group_by(gbd_ggw_code, gbd_ggw_naam) |>
    my_mutate()|>
    add_column(pdg_naam = 'NDG_totaal')
)


# frequenties van herkomstsd naar afzetsd
freq_kkb_24$SD_sd <- bind_rows (
  
  freq_kkb_24$SD |>
    group_by(pdg_naam, gbd_sdl_code, gbd_sdl_naam, afzet_stadsdeel_code  ) |>
    my_summary()|>
    group_by(pdg_naam, gbd_sdl_code, gbd_sdl_naam) |>
    my_mutate(),
  
  freq_kkb_24$SD |>
    filter(pdg_naam != 'dagelijkse producten') |>
    group_by(gbd_sdl_code, gbd_sdl_naam, afzet_stadsdeel_code  ) |>
    my_summary()|>
    group_by(gbd_sdl_code, gbd_sdl_naam) |>
    my_mutate()|>
    add_column(pdg_naam = 'NDG_totaal')
)

saveRDS(    freq_kkb_24, "03 tussentijds/freq_kkb_24.rds")

# is afgerond   
write.xlsx(freq_kkb_24, "04 output tabellen/tabel_kkb_24.xlsx", overwrite = T, withFilter=T )



