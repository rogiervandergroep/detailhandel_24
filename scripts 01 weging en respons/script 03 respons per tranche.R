
# libraries en os-functies -
source("03 R scripts/scripts 01 weging en respons/script 00 setup.R")

# steekproefkader -
source("03 R scripts/scripts 01 weging en respons/script 02 steeksproefkader en respons tabellen.R")

### inlezen data ---
temp <- "00 ruwe data 2022 2023"

filenames_f <- list.files(temp, pattern="*.sav", full.names=TRUE)

filenames_s <- list.files(temp, pattern="*.sav", full.names=FALSE)
filenames_s <- str_remove (filenames_s, "_gecodeerd.sav")
filenames_s <- str_replace(filenames_s, "230349", "tranche")

files <- map(filenames_f, read_sav)
names(files)<- filenames_s



### respons per tranche ---

my_respons<- function(x) {
  
  x |>
    group_by(gbd_wijk_code)|>
    summarise(respons=n()) 
  
}

respons  <- files   |>
  map  (my_respons) |>
  map2 (filenames_s, \(x, y) add_column(x, tranche = y)) |>
  bind_rows() |>
  pivot_wider(names_from = tranche, values_from = respons)


# nieuwe steekproefkade tranche 2:

my_newtarget<- function (x, y) {
  
  respons <- respons |>
    select(all_of(c("gbd_wijk_code", y)))
  
  last_col <- names(x[ , ncol(x), drop = FALSE])    

  x |> 
    left_join(respons, by=c("wijk_code"="gbd_wijk_code")) |>
    
    mutate("{y}"  := replace_na(.data[[y]], 0),
           "tekort_{noquote(y)}" := .data[[last_col]] - .data[[y]],
           "target_nieuw_{noquote(y)}" := (.data[[last_col]] - .data[[y]]) + target_t_basis )  
    
   # |> mutate(across(where(is.numeric), ~ (., digits =0)))
  

  
  }
 
steekproefkader_t3 <- reduce(filenames_s, \(steekproefkader, y) my_newtarget(steekproefkader, y), .init = steekproefkader)


steekproefkader_t3 <- steekproefkader_t3 |>
  mutate(steek_t4=case_when(target_nieuw_tranche_3 <  2 ~ 0,
                            target_nieuw_tranche_3 >= 2 & target_nieuw_tranche_3 < 5 ~ target_nieuw_tranche_3*7,
                            target_nieuw_tranche_3 >= 5 ~ target_nieuw_tranche_3*8)
         
         )
write.xlsx(steekproefkader_t3, "01 steekproefkader en respons/230349 basis voor steekproeftrekking 2022 2023 steek 4 panel.xlsx")

