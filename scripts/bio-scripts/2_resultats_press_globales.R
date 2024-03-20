#------------------------------------------- RED LIST ASSESSMENT --------------------------------------------------#

# 0-/ Run le code : resultats_press_locales

rm(list=ls())
library(dplyr)
library(tidyr)
library(readxl)

Q_abs <- readRDS("data/rds/Q_abs.rds")
redlist_press <- readRDS("data/rds/redlist_press.rds")
press_globales <- readRDS("data/rds/press_globales.rds")
result <- readRDS("data/rds/result.rds")
r1 <- readRDS("data/rds/r1.rds")

###############################################################################
##### VIII-/ Pressions globales : Calcul du score de risque d'extinction  #####
###############################################################################

# Pour les pressions globales on répartit la responsabilité entre tous les secteurs qui générent 
# la pression quant bien même l'espèce n'est pas présente dans leur pays

### 1-/ Calculer la part de pressions globales généré par les secteurs 

Q_globales <- Q_abs %>% group_by(Lfd_Nr) %>% mutate(part_press_g=pressure/sum(pressure)) %>% select(-pressure) %>% 
  filter(Lfd_Nr %in% press_globales)

### 2-/ Couplage pressions globales et score STARij 

redlist_press_globales <- redlist_press %>% filter(Lfd_Nr %in% result$Lfd_Nr)

result_g <- r1 %>% ungroup() %>% select(taxonid,Lfd_Nr,STARij) %>% distinct() %>% filter(Lfd_Nr %in% press_globales) %>%
  left_join(Q_globales, by="Lfd_Nr") %>% mutate(score=STARij*part_press_g)

rm(Q_abs,Q_globales,r1,redlist_press,redlist_press_globales,result,press_globales)

resultats_dt <- data.table::as.data.table(result_g)

rm(result_g)

resultats_pressions_globales <- resultats_dt[, .(score = sum(score)), by = .(taxonid, sector, country, iso)]

rm(resultats_dt)

# saveRDS(resultats_pressions_globales,"data/rds/resultat_press_globales.rds")
