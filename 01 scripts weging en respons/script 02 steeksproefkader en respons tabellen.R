
#lees libraries in met setup
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")


### aantal inwoners nieuwe wijkindeling 22


# inlezen pc6 en wijkdata
pc6wijk22 <- read.csv2("G:/OIS/Internezaken/Techniek/SPSS/0_POSTCODE/pc6_gebiedsindeling2022_ontdubbeld_20220401_NW.csv")  |>
  mutate(gbd_wijk_code= replace_na(gbd_wijk_code, "NA"))

# lijst met alle nieuwe wijken en gebieden
wijk22<- pc6wijk22 |>
  group_by(gbd_wijk_code, gbd_wijk_naam, gbd_ggw_code, gbd_ggw_naam)|>
  summarise(aantal=n())|>
  select(gbd_wijk_code:gbd_ggw_naam)

# koppel gebiedsindeling aan inwoner per wijkdata
steekproefkader <- read.xlsx("02 lookup tabellen/bevolking_naar_wijk_Rogier.xlsx")|>
  mutate(wijk_code= replace_na(wijk_code, "NA"))|>
  rename(inwoners22=aantal)|>
  left_join(wijk22, by=c("wijk_code"= "gbd_wijk_code"))|>

# bepaal target per tranche: 200 per gebied, kleine gebieden: 50 per gebied  
# NB: Sloterdijk FA is een gebied GF06, dus 50 is te hoog, handmatig op 10 gezet.

  group_by(gbd_ggw_code,  gbd_ggw_naam)|>
  mutate(inw_geb22 = sum(inwoners22))|>
  mutate(target_totaal=
           case_when(inw_geb22>20000                   ~ inwoners22/inw_geb22*200,
                     inw_geb22>5000 & inw_geb22<=20000 ~ inwoners22/inw_geb22*50,
                     inw_geb22<=5000                   ~ inwoners22/inw_geb22*10)
         )|>
  
  # omdat er vier veldwerktranches zijn (waarvan een via panel)
  
  mutate(target_t_basis = target_totaal/4)|>
  
# verwijder Westelijk Havengebied
  filter(gbd_ggw_code !='ZX99')

# check totale target en target 1
steekproefkader|>
  ungroup()|>
  summarise(target_totaal=sum(target_totaal),
            target_t_basis=sum(target_t_basis))

saveRDS(steekproefkader, file = "03 output tabellen/steekproefkader.rds")

