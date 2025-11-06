#------------------------------------------- RED LIST ASSESSMENT --------------------------------------------------#

# 0-/ Run le code : resultats_press_locales

rm(list=ls())
gc()
library(dplyr)
library(tidyr)
library(readxl)

setwd('~/projects/AFD/')

# set_wd <- "data/bio/rds"
# bucket <- "siwar"

Q_abs <- readRDS("rds/Q_abs.rds")

redlist_press <- readRDS("rds/redlist_press.rds")

press_globales <- readRDS("rds/press_globales.rds")

result <- readRDS("rds/result-v2.rds")

r1 <- readRDS("rds/r1-v2.rds")


###############################################################################
##### VIII-/ Pressions globales : Calcul du score de risque d'extinction  #####
###############################################################################

# Pour les pressions globales on répartit la responsabilité entre tous les secteurs qui générent 
# la pression quant bien même l'espèce n'est pas présente dans leur pays

### 1-/ Calculer la part de pressions globales généré par les secteurs 

Q_globales <- Q_abs %>%
  group_by(Lfd_Nr) %>%
  mutate(part_press_g=pressure/sum(pressure)) %>%
  select(-pressure) %>% 
  filter(Lfd_Nr %in% press_globales)

### 2-/ Couplage pressions globales et score STARij 

redlist_press_globales <- redlist_press %>%
  filter(Lfd_Nr %in% result$Lfd_Nr)

result_g <- r1 %>% 
  ungroup() %>% 
  select(taxonid,Lfd_Nr,STARp) %>% 
  distinct() %>% 
  filter(Lfd_Nr %in% press_globales) %>%
  left_join(Q_globales, by="Lfd_Nr", relationship = "many-to-many") %>% 
  mutate(score=STARp*part_press_g)

rm(Q_abs,Q_globales,r1,redlist_press,redlist_press_globales,result,press_globales)

resultats_dt <- data.table::as.data.table(result_g)

rm(result_g)

resultats_pressions_globales <- resultats_dt[, .(score = sum(score)), by = .(taxonid, sector, country, iso)]

resultats_pressions_globales_press <- resultats_dt[, .(score = sum(score)), by = .(Lfd_Nr, sector, country, iso)]

rm(resultats_dt)

saveRDS(resultats_pressions_globales_press, "rds/resultat_press_globales_press-v2.rds")

saveRDS(resultats_pressions_globales, "rds/resultat_press_globales-v2.rds")

# saveRDS(resultats_pressiTRUE# saveRDS(resultats_pressions_globales,"data/rds/resultat_press_globales.rds")

# saveRDS(resultats_pressions_globales_press,"data/rds/resultat_press_globales_press.rds")
