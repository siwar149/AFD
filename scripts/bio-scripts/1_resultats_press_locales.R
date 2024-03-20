#------------------------------------ RED LIST ASSESSMENT --------------------------------------------------

rm(list=ls())
library(dplyr)
library(tidyr)
library(readxl)

setwd(dir="/home/onyxia/work/R_light/")

bucket = "siwar"
set_wd <- "data/bio/rds"

####################################################################
### I-/ Trie des données redlist - Méthode d'Irwin et al. (2022) ###
####################################################################

### 1-/ Les données brutes de la redlist

#rd_threats <- readRDS("data/rds/redlist_threats_to_species.rds") %>% filter(!id=="TRUE") %>% mutate(id = as.integer(id)) # data menaces IUCN de la redlist
#rd_species <- readRDS("data/rds/Red_list_species.rds") # data espèces de la redlist

rd_threats <- s3read_using(FUN = readRDS,
             object = paste(set_wd,"/redlist_threats_to_species.rds",sep=""),
             bucket = bucket, opts = list("region" = "")) %>% 
             mutate(id = as.integer(id)) # data menaces IUCN de la redlist

rd_species <- s3read_using(FUN = readRDS,
                           object = paste(set_wd,"/Red_list_species.rds",sep=""),
                           bucket = bucket, opts = list("region" = ""))

rd_source <- rd_species %>% 
             select(taxonid, scientific_name,category,class_name) %>%
             left_join(rd_threats,by=c("taxonid"="id")) # data complète des informations de la redlist
length(unique(rd_source$taxonid)) # il y a 153,732 espèces au total

### 2-/ On retire les espèces qui n'ont pas de menaces de renseignées

rd <- rd_source %>% drop_na(result.code)
length(unique(rd$taxonid)) # 82,366 espèces avec une catégorie de menace renseignée

### 3-/ On ne garde que les espèces d'amphibiens, les oiseaux et les mammiféres 
#(peut-être pourrait-on garder les reptiles? source: https://www.iucnredlist.org/resources/summary-statistics#Summary%20Tables)

rd <- rd %>% filter(class_name %in% c("AVES","MAMMALIA","AMPHIBIA")) 
length(unique(rd$taxonid)) # 13,525 espèces restantes

### 4-/ On ne garde que les catégories de menaces élevés : CR/EN/VU/NT

rd <- rd %>% filter(category %in% c("CR","VU","EN","NT"))
length(unique(rd$taxonid)) # 7,374 espèces avec un niveau de criticité élevé

### 5-/ On retire les espèces qui peuvent avoir des catégories de menaces différentes 

rd <- rd %>% group_by(taxonid) %>% filter(n_distinct(category) == 1) %>% ungroup()
length(unique(rd$taxonid)) # toujours 7,374 espèces

### 6-/ On ne garde que les timing de menace "Ongoing" ou "Future"

rd <- rd %>% filter(result.timing %in% c("Future","Ongoing"))
length(unique(rd$taxonid)) # 7,340 espèces restantes

### 7-/ Eviter un double comptage des menaces niveau 2 (2 chiffres) et 3 (3 chiffres):
# suppréssion de toutes les menaces de 2 chiffres à moins que la menace de 3 chiffres ne soit pas renseignée

rd$level <- nchar(gsub("\\D", "", rd$result.code))
rd$TG <- substr(rd$result.code, 1, 3)

rd <- rd %>% group_by(TG, taxonid) %>% filter(!(level == 2 & any(level == 3))) %>% ungroup() %>% select(-level,-TG) %>% ungroup()

rm(rd_source,rd_threats,rd_species)


####################################################################
### II-/ Calculer les scores STAR - Méthode d'Irwin et al. 2022 ####
####################################################################

### 1-/ Attribution d'un score pour chaque catégorie de menaces

STAR <- rd %>% mutate(categ.score = case_when(category == "NT" ~ 1,
                                              category == "VU" ~ 2,
                                              category == "EN" ~ 3,
                                              category == "CR" ~ 4,
                                              TRUE ~ NA_real_))

### 2-/ Attribution d'un score en fonction du niveau de sévérité et du scope de la menace

STAR <- STAR %>% mutate(TS = case_when(
  result.scope == "Minority (<50%)" & result.severity %in% c("Negligible declines", "No decline") ~ 0,
  result.scope == "Minority (<50%)" & result.severity %in% c("Slow, Significant Declines", "Causing/Could cause fluctuations") ~ 5,
  result.scope == "Minority (<50%)" & result.severity == "Rapid Declines" ~ 7,
  result.scope == "Minority (<50%)" & result.severity == "Very Rapid Declines" ~ 24,
  
  result.scope == "Majority (50-90%)" & result.severity %in% c("Negligible declines", "No decline") ~ 0,
  result.scope == "Majority (50-90%)" & result.severity %in% c("Slow, Significant Declines", "Causing/Could cause fluctuations") ~ 9,
  result.scope == "Majority (50-90%)" & result.severity == "Rapid Declines" ~ 18,
  result.scope == "Majority (50-90%)" & result.severity == "Very Rapid Declines" ~ 52,
  
  result.scope == "Whole (>90%)" & result.severity %in% c("Negligible declines", "No decline") ~ 1,
  result.scope == "Whole (>90%)" & result.severity %in% c("Slow, Significant Declines", "Causing/Could cause fluctuations") ~ 10,
  result.scope == "Whole (>90%)" & result.severity == "Rapid Declines" ~ 24,
  result.scope == "Whole (>90%)" & result.severity == "Very Rapid Declines" ~ 63, TRUE ~ NA_real_))

### 3-/ Attribution de la valeur médiane quand le scope et la sévérité ne sont pas renseignés

# Quand result.scope == "Unknown"

median_no_decline <- STAR %>%  filter(result.severity == "Negligible declines" | result.severity == "No decline") %>%  summarize(median_TS = median(TS, na.rm = TRUE)) %>% pull() 
median_slow_decline <- STAR %>%  filter(result.severity == "Slow, Significant Declines") %>%  summarize(median_TS = median(TS, na.rm = TRUE)) %>% pull()
median_rap_decline <- STAR %>%  filter(result.severity == "Rapid Declines") %>%  summarize(median_TS = median(TS, na.rm = TRUE)) %>% pull()
median_very_rapdecline <- STAR %>%  filter(result.severity == "Very Rapid Declines") %>%  summarize(median_TS = median(TS, na.rm = TRUE)) %>% pull()

STAR <-  STAR %>% dplyr::mutate(TS=if_else((is.na(result.scope) | result.scope == "Unknown") & (result.severity == "Negligible declines" | result.severity == "No decline"),median_no_decline,
                                   if_else((is.na(result.scope) | result.scope == "Unknown") & (result.severity == "Slow, Significant Declines" | result.severity == "Causing/Could cause fluctuations"),median_slow_decline,
                                   if_else((is.na(result.scope) | result.scope == "Unknown") & result.severity == "Rapid Declines", median_rap_decline,
                                   if_else((is.na(result.scope) | result.scope == "Unknown") & result.severity == "Very Rapid Declines",median_very_rapdecline, TS)))))

# Quand result.severity == "Unknown"

median_mino <- STAR %>%  filter(result.scope == "Minority (<50%)") %>%  summarize(median_TS = median(TS, na.rm = TRUE)) %>% pull() 
median_majo <- STAR %>%  filter(result.scope == "Majority (50-90%)") %>%  summarize(median_TS = median(TS, na.rm = TRUE)) %>% pull()
median_whole <- STAR %>%  filter(result.scope == "Whole (>90%)") %>%  summarize(median_TS = median(TS, na.rm = TRUE)) %>% pull()

STAR <-  STAR %>% dplyr::mutate(TS=if_else((is.na(result.severity) | result.severity == "Unknown") & result.scope == "Minority (<50%)", median_mino,
                                   if_else((is.na(result.severity) | result.severity == "Unknown") & result.scope == "Majority (50-90%)", median_majo,
                                   if_else((is.na(result.severity) | result.severity == "Unknown") & result.scope == "Whole (>90%)", median_whole, TS))))

# Quand result.scope == "Unknown" & result.severity == "Unknown"

STAR <-  STAR %>% dplyr::mutate(TS=if_else((is.na(result.scope) | result.scope == "Unknown") & (is.na(result.severity) | result.severity == "Unknown"),
                                           median(TS, na.rm = TRUE),TS))

STAR <- STAR %>% dplyr::mutate(STARij=TS*categ.score) # calcul du score STARij

rm(list = grep("^median", ls(), value = TRUE),rd)


##################################################################
### III-/ Répartition des espèces dans le monde  #################
##################################################################

### 1-/ Base de données des range des aires de répartition des espèces d'amphibiens et de mammifères terrestres

mammal_source <- read_excel("data/species_range/terrestrial_mammals_range_data.xlsx") %>% select(id_no,Surf_sp, iso3,name) # beaucoup de régions n'ont pas d'iso car zones de conflit entre plusieurs pays 
amphi_source1 <- read_excel("data/species_range/anura_range_data.xlsx") %>% select(id_no,Surf_sp, iso3,name)
amphi_source2 <- read_excel("data/species_range/Caudata_range_data.xlsx") %>% select(id_no,Surface_sp, iso3,name) %>% rename(Surf_sp=Surface_sp)
amphi_source3 <- read_excel("data/species_range/Gymnophiona_range_data.xlsx") %>% select(id_no,surf_sp, iso3,name) %>% rename(Surf_sp=surf_sp)
bird_source <- read_excel("data/species_range/Birds.xlsx") %>% select(sisid,Surf_sp,iso3,name) %>% rename(id_no=sisid)

species_range <- rbind(mammal_source,amphi_source1,amphi_source2,amphi_source3,bird_source)

### 2-/ Table de concordance entre la nomenclature de pays gloria et celle des géographies d'espèces (=olson)

match_pays_olson_gloria <- read_excel("data/match_pays_olson_gloria.xlsx", sheet = "analyse_R_v2")  
match_pays_olson_gloria[match_pays_olson_gloria$pays_o == "CÃ´te d'Ivoire", "pays_o"] <- "Côte d'Ivoire" # encoding error for CIV

### 3-/ On ne garde que les espèces de la redlist filtrée 

species <- species_range %>% filter(id_no %in% STAR$taxonid) %>% drop_na(iso3) %>% group_by(id_no) #%>% mutate(range=Surf_sp/sum(Surf_sp))
length(unique(species$id_no)) # On obtient 4 592 espèces 

label_IO <- as.data.frame(readRDS("data/rds/label_IO.rds")) %>% rename(iso=V1, country=V2, sector=V3)

species <- species %>% left_join(match_pays_olson_gloria, by=c("name"="pays_o"),relationship = "many-to-many") %>%
  select(-iso3,-name) %>% group_by(pays_g,id_no) %>% summarise(range=sum(Surf_sp)) %>% 
  left_join(label_IO, by=c("pays_g"="country"),relationship = "many-to-many") %>% drop_na(pays_g) %>% rename(taxonid=id_no) %>% 
  group_by(taxonid,iso,sector)

length(unique(species$taxonid)) # 4 590 espèces pour lesquelles on a une aire de répartition

### 4-/ On ne garde que les espèces qui ont une aire de répartition dans la data STAR

STAR <- STAR %>% filter(taxonid %in% species$taxonid)

rm(mammal_source,amphi_source1,amphi_source2,amphi_source3,match_pays_olson_gloria,species_range,bird_source)


###########################################################
### IV-/ Couplage menaces IUCN & pressions gloria  ########
###########################################################

### 1-/ Data de biotope correspondance menaces IUCN & pressions gloria (en deux partie car certaines colonnes sont répétés dans la base de donnée de Biotope + exclusion des informations hors tableau)

biotope_source1 <- read_excel("data/Biotope-Livrable_Equivalence_table_GLORIA.xlsx", sheet = "1. Equivalence Table",range = cell_limits(c(3,1),c(162,113)))
biotope_source2 <- read_excel("data/Biotope-Livrable_Equivalence_table_GLORIA.xlsx", sheet = "1. Equivalence Table",range = cell_limits(c(3,120),c(162,125)))
biotope_source <- cbind(biotope_source1,biotope_source2)

biotope_source <- biotope_source %>% rename_all(~ gsub("[^0-9.]", "", .)) %>% rename(pressure = !!names(.)[1]) %>% mutate_at(vars(-1), as.numeric)
biotope_source <- biotope_source %>% pivot_longer(!pressure, names_to = "threat", values_to = "corr") %>% filter(corr==1) %>% select(-corr) # on ne garde que les correspondances positives

### 2-/ Faire correspondre les pressions analysés dans biotope avec celles de gloria (écriture différente + nouvelle version gloria)

corr_press <- read_excel("data/gloria_55_57_pressures.xlsx", sheet = "Final_Corr") %>% drop_na(Sattelites_biotope)

biotope <- biotope_source %>% left_join(corr_press %>% select(1,7) %>% distinct(), by=c("pressure"="Sattelites_biotope"),relationship = "many-to-many") %>% 
  drop_na(Lfd_Nr) %>% distinct() 

### 3-/ Eviter un double comptage des menaces niveau 2 (2 chiffres) et 3 (3 chiffres): suppréssion de toutes les menaces de 2 chiffres à moins que la menace de 3 chiffres ne soit pas renseignée

biotope$level <- nchar(gsub("\\D", "", biotope$threat))
biotope$TG <- substr(biotope$threat, 1, 3)

biotope <- biotope %>% group_by(TG) %>% filter(!(level == 2 & any(level == 3))) %>% ungroup() %>% select(-level,-TG) %>% ungroup()

rm(biotope_source1,biotope_source2,corr_press,biotope_source)


########################################
### V-/ Pressions gloria couplage  #####
########################################

### 1-/ Données satéllitaires de gloria en valeur absolues pour l'année 2019

Q <- readRDS("data/rds/QT_2019.rds") 
label_Q <- as.data.frame(readRDS("data/rds/label_Q.rds"))

### 2-/ Identifier les pressions analysables

pressions_gloria <- data.frame(sum=rowSums(Q)) %>% cbind(label_Q) %>% tibble::rownames_to_column("Lfd_Nr") %>% filter(!sum==0) %>% select(Lfd_Nr) %>% distinct() %>% 
  mutate(Lfd_Nr = as.numeric(Lfd_Nr)) # Pressions analysables dans gloria

pressions_biotope <- biotope %>% select(Lfd_Nr) %>% distinct() # Pressions analysables dans biotope 

pressions_analysables <- inner_join(pressions_gloria,pressions_biotope) %>% pull() # 112 pressions analysables

### 3-/ Filter les pressions analysables dans Q

Q_abs <- Q[pressions_analysables,]
rownames(Q_abs) <- pressions_analysables

Q_abs <- as.data.frame(Q_abs) %>% tibble::rownames_to_column("Lfd_Nr") %>% pivot_longer(!Lfd_Nr,names_to="pays_secteur",values_to="pressure")

label_pays_secteur <- label_IO %>% mutate(pays_secteur=paste0("V",1:n()))

Q_abs <- Q_abs %>% left_join(label_pays_secteur) %>% filter(!pressure==0) %>% select(-pays_secteur) %>% mutate(Lfd_Nr=as.numeric(Lfd_Nr))

rm(Q,pressions_biotope,pressions_gloria,label_pays_secteur)


######################################################
### VI-/ Couplage redlist et pressions  ##############
######################################################

### 1-/ Retirer de la liste de biotope les pressions non analysables 

redlist_press <- STAR %>% select(taxonid,result.code,STARij) %>% left_join(biotope,by=c("result.code"="threat")) %>% filter(Lfd_Nr %in% pressions_analysables) %>%
  select(-pressure) %>% filter(STARij>0)

rm(STAR,biotope)

###############################################################################
##### VII - Pressions locales : Calcul du score de risque d'extinction  ######
###############################################################################

# Pour les pressions locales on répartit la responsabilité entre tous les secteurs qui possaident 
# l'espèce menacée par la pression locale en tenant compte de l'aire de répartition de l'espèce dans le pays donné 

### 1-/ Créer un vecteur de pressions globales

press <- label_Q %>% tibble::rownames_to_column("Lfd_Nr") %>% filter(Lfd_Nr %in% pressions_analysables) %>%
  filter(Sat_head_indicator=="Energy" | Sat_head_indicator=="Emissions (EDGAR)")

AR6 <- read_excel("data/CO2_AR6.xlsx") %>% rename(Sat_indicator=Formula) # gaz à effet de serre d'après l'AR6

press <- press %>% mutate(Sat_indicator = gsub("_total_EDGAR_consistent'|'c_|'|_10_mee|_excl_short_cycle_org_c_total_EDGAR_consistent |_org_short_cycle_c_total_EDGAR_consistent", "", Sat_indicator))

press_globales <- AR6 %>% left_join(press) %>% drop_na(Lfd_Nr) %>% mutate(Lfd_Nr=as.numeric(Lfd_Nr)) %>% arrange(Lfd_Nr) %>% pull(Lfd_Nr) 

### 2-/ Ajouter les secteurs dans la data d'aire de répartition

repartition <- label_IO %>% left_join(species) %>% drop_na(range) 
repartition <- subset(repartition, taxonid %in% redlist_press$taxonid) # on ne garde que les 4 210 espèces qui ont une menace reliable à un secteur

### 3-/ Calculer la part de pression générée par les secteurs qui possaident l'espèce 

press_locales <-setdiff(pressions_analysables, press_globales) # créer un vecteur de pressions locales

result <- redlist_press %>% left_join(repartition)
result <- result %>% left_join(Q_abs) %>% drop_na(pressure)

rm(label_IO,AR6,label_Q,species)

r1_w_ex <- result %>% group_by(result.code,taxonid) %>% mutate(nbr_press_per_threat=length(unique(Lfd_Nr))) %>%
  mutate(STARij=STARij/nbr_press_per_threat) %>% ungroup() %>% group_by(taxonid,Lfd_Nr,iso,country,sector,range,pressure) %>% summarise(STARij=sum(STARij)) %>% 
  group_by(taxonid,Lfd_Nr,iso) %>% 
  mutate(part_press=pressure/sum(pressure)) 

r_p <- r1_w_ex %>% ungroup() %>% select(taxonid,Lfd_Nr,iso,range) %>% distinct() %>% group_by(taxonid,Lfd_Nr) %>% summarise(per_range=range/sum(range),iso) # range per pressures 

ex_w <- r1_w_ex %>% left_join(r_p) %>% mutate(score=STARij*per_range*part_press) %>% filter(Lfd_Nr %in% press_locales)
 
resultats_pressions_locales <- ex_w %>% 
group_by(taxonid,sector,country,iso) %>% summarise(score=sum(score), .groups = 'drop')

# saveRDS(resultats_pressions_locales,"data/rds/resultats_pressions_locales.rds")

# saveRDS(Q_abs,"data/rds/Q_abs.rds")
# saveRDS(redlist_press,"data/rds/redlist_press.rds")
# saveRDS(press_globales,"data/rds/press_globales.rds")
# saveRDS(result,"data/rds/result.rds")
# saveRDS(r1_w_ex,"data/rds/r1.rds")
 