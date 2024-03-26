rm(list=ls())

library(dplyr)

########################################################################
##### IX - Combiner l'analyse des pressions globales et locales ########
########################################################################

#resultats_pressions_locales <- readRDS("data/rds/resultats_pressions_locales.rds")
#resultats_pressions_globales <- readRDS("data/rds/resultat_press_globales.rds")

resultats_pressions_locales <- s3read_using(FUN = readRDS,
             object = paste(set_wd,"/resultats_pressions_locales.rds",sep=""),
             bucket = bucket, opts = list("region" = ""))

resultats_pressions_globales <- s3read_using(FUN = readRDS,
             object = paste(set_wd,"/resultat_press_globales.rds",sep=""),
             bucket = bucket, opts = list("region" = ""))

### 1-/  Résultats à la maille espèces/ secteur/ pays

m <- rbind(resultats_pressions_globales, resultats_pressions_locales)
merged_data <- data.table::as.data.table(m)
merged_data <- merged_data[, .(score_sum = sum(score)), by = .(taxonid, sector, country, iso)]

rm(resultats_pressions_globales,resultats_pressions_locales,m)

   #################
   # test si la somme des scores d'espèces = score STARij tot d'une espèce en fonction des pressions renseignées 
   
   #r1 <- readRDS("data/rds/r1.rds") 
   #redlist_press <- readRDS("data/rds/redlist_press.rds")
   
   redlist_press <- s3read_using(FUN = readRDS,
                                 object = paste(set_wd,"/redlist_press.rds",sep=""),
                                 bucket = bucket, opts = list("region" = ""))
   
   r1 <- s3read_using(FUN = readRDS,
                      object = paste(set_wd,"/r1.rds",sep=""),
                      bucket = bucket, opts = list("region" = ""))
   
   
   pressures_analysed <- r1 %>% ungroup() %>% select(taxonid,Lfd_Nr) %>% distinct()
   
   test_STAR <- pressures_analysed %>% left_join(redlist_press) %>% group_by(taxonid,result.code) %>% 
     mutate(nbr_press=length(Lfd_Nr)) %>% mutate(STARij=STARij/nbr_press) %>% group_by(taxonid) %>%
     summarise(STARij=sum(STARij))
   
   test_score <- merged_data %>% group_by(taxonid) %>% summarise(score=sum(score_sum))
   
   test <- test_STAR %>% left_join(test_score) %>% mutate(equiv=STARij-score) 
   #################

# saveRDS(merged_data, "data/rds/redlist_score_per_species.rds")
   
   ### 2-/  Résultats à la maille secteur/ pays

score_pays <- merged_data %>% group_by(iso,sector) %>% summarise(score=sum(score_sum))  
  
# saveRDS(score_pays,"data/rds/score_pays.rds")
