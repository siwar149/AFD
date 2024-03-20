rm(list=ls())

library(dplyr)
library(maps)
library(mapproj)
library(ggplot2)
library(readxl)
library(ggridges)

########################################################################
##### X - Analyse des scores de Red List ###############################
########################################################################

score_species <- readRDS("data/rds/redlist_score_per_species.rds")
score <- readRDS("data/rds/score_pays.rds")

summary(score_species$score_sum) 
summary(score$score) 

#######
# Species analysis
#######

species <- score_species %>% group_by(taxonid) %>% summarise(score=sum(score_sum)) %>% filter(score>0)

length(species$taxonid) # 4,268 species analysed 

species_info <- readRDS("data/rds/Red_list_species.rds")

species <- species %>% left_join(species_info) %>% group_by(class_name) %>% mutate(n=length(class_name))

table(species$class_name) # 2 729 amphibiens ; 1 432 mammifères ; 107 oiseaux 

ggplot(species, aes(x = reorder(class_name, -n))) +
  geom_bar() +
  labs(title = "Number of species per taxon") +
  labs(x="",y="") +
  theme_minimal()

ggplot(species) + geom_density(aes(x=score,fill=class_name)) + facet_wrap(vars(class_name),nrow=3) + theme_ridges() + theme(legend.position = "none")

#######
# World map
#######

world_map <- map_data("world") %>% 
  filter(! long > 180) 

match_pays_gloria_WM <- read_excel("data/match_pays_gloria_WM.xlsx", 
                                   sheet = "WM_Olson_gloria")

country_score <- score_species %>%  group_by(country) %>% summarize(score=sum(score_sum)) %>% distinct() %>% left_join(match_pays_gloria_WM, by=c("country"="pays_g")) 
map <- world_map %>% left_join(country_score,by=c("region"="WM"))

map %>%
  ggplot(aes(map_id = region, fill = score)) +
  geom_map(map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               color = "black", fill = NA, size=0.1) +
  labs(fill = "Redlist score", fontface = "bold") +
  scale_fill_gradient(low = "green", high = "red") +
  coord_map("moll") +
  theme(plot.title = element_text(hjust = 0.5)) 

#######
# Boxplot sector
#######

BP <- score %>%
  group_by(sector) %>%
  dplyr::mutate(med = median(score)) 

ggplot(BP, aes(y = score, x = reorder(sector, -med))) +
  geom_boxplot(notch = FALSE, outlier.shape = NA, fill = '#FF8080', color = "black") +
  labs(x = "", y = "") +
  theme_bw() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, fill = "#63416A", color = "#63416A") +
  coord_cartesian(ylim = c(0, 50)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#######
# Min/Max normalization
#######

species <- species %>% mutate(score_scaled = (score - min(score)) / (max(score) - min(score))) # min / max normalization de toute la colonne score