#------------------------------------------- RED LIST ASSESSMENT --------------------------------------------------#

# 0-/ Run le code : resultats_press_locales

rm(list=ls())
library(dplyr)
library(tidyr)
library(readxl)

set_wd <- "data/bio/rds"
bucket <- "siwar"

#Q_abs <- readRDS("data/rds/Q_abs.rds")

Q_abs <- s3read_using(FUN = readRDS,
                      object = paste(set_wd,"/Q_abs.rds",sep=""),
                      bucket = bucket, opts = list("region" = ""))

#redlist_press <- readRDS("data/rds/redlist_press.rds")

redlist_press <- s3read_using(FUN = readRDS,
                              object = paste(set_wd,"/redlist_press.rds",sep=""),
                              bucket = bucket, opts = list("region" = ""))

#press_globales <- readRDS("data/rds/press_globales.rds")

press_globales <- s3read_using(FUN = readRDS,
                              object = paste(set_wd,"/press_globales.rds",sep=""),
                              bucket = bucket, opts = list("region" = ""))

#result <- readRDS("data/rds/result.rds")

result <- s3read_using(FUN = data.table::fread,
                       object = paste(set_wd,"/result-v2.rds",sep=""),
                       bucket = bucket, opts = list("region" = ""))

#r1 <- readRDS("data/rds/r1.rds")

r1 <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd,"/r1-v2.rds",sep=""),
                   bucket = bucket, opts = list("region" = ""))

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
  left_join(Q_globales, by="Lfd_Nr") %>% 
  mutate(score=STARp*part_press_g)

rm(Q_abs,Q_globales,r1,redlist_press,redlist_press_globales,result,press_globales)

resultats_dt <- data.table::as.data.table(result_g)

rm(result_g)

resultats_pressions_globales <- resultats_dt[, .(score = sum(score)), by = .(taxonid, sector, country, iso)]

#resultats_pressions_globales_press <- resultats_dt[, .(score = sum(score)), by = .(Lfd_Nr, sector, country, iso)]

rm(resultats_dt)

s3write_using(x = as.data.table(resultats_pressions_globales_press), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd,"/resultat_press_globales_press-v2.rds",sep=""),
              bucket = bucket, opts = list("region" = ""))

s3write_using(x = as.data.table(resultats_pressions_globales), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd,"/resultat_press_globales-v2.rds",sep=""),
              bucket = bucket, opts = list("region" = ""))

# saveRDS(resultats_pressiTRUE# saveRDS(resultats_pressions_globales,"data/rds/resultat_press_globales.rds")

# saveRDS(resultats_pressions_globales_press,"data/rds/resultat_press_globales_press.rds")
