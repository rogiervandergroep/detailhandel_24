

# wegingsfactoren 

# terugwegen van respons naar daadwerkelijke respons

# stap 1 bepaal de verhouding inwoners per wijk

inw_wijk <- steekproefkader[,c("wijk_code",  "inwoners22")]%>%
  ungroup()%>%
  mutate(inw22_aandeel=inwoners22/sum(inwoners22))


library(rlang)


functie_weegfactor<- function(.x, resp) {
  
  .x%>%
    mutate(
      respons_aandeel= {{ resp }}/sum( {{ resp }} )) %>%
    left_join(
      inw_wijk, by=c("gbd_wijk_code"="wijk_code"))%>%
    mutate(
      weegfactor=inw22_aandeel/respons_aandeel)
 
}

# stap 2 bepaal verhouding respons per wijk
resp_t1_wijk22_wf <- resp_t1_wijk22%>%
  functie_weegfactor(respons_t1)


# voeg nieuwe wijkindeling toe aan dataset 
data22_wg <- data22 %>% 
  left_join(pc6wijk22, by=c("postcode"))%>%
  left_join(resp_t1_wijk22_wf, by=c("gbd_wijk_code"))

#check data
#write.table(resp_t1_wijk22_wf,"clipboard")


# tabellen met rechte tellingen per stadsdeel en per gebied 

koopstromen_SD <- data22_wg %>%
  group_by(gbd_sdl_code)%>%
  summarise(v1_dg= sum(V1_nw*weegfactor))
  



koopstromen_GEB
