##### Make some graphs ####

install.packages("viridis")  # Install
library("viridis")           # Load
install.packages("ggsci")
library("ggsci")
install.packages("mapproj")
library("mapproj")



bucket1 = "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 = "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


g <- s3read_using(FUN = data.table::fread,
                        object = paste(set_wd2,"/g_2_2019.rds",sep=""),
                        bucket = bucket2, opts = list("region" = ""))


g1 <- g %>%
  group_by(iso, country, eu) %>%
  mutate(revarx= abs(dx) / sum(x) * 100)


g1 <- g1 %>%
  group_by(iso, country, eu) %>%
  arrange(desc(revarx)) %>%
  mutate(across(where(is.numeric), ~ ifelse(row_number() >= 6, sum(.), .))) %>%
  mutate(NACE = ifelse(row_number() == 6, "X", NACE)) %>%
  filter(row_number() <= 6)




ggplot(g1, aes(x = eu, y = revarx, fill = factor(NACE))) +
  geom_bar(stat = "identity", color = "black") +  # Adding lines to each filled sector
  labs(x = "EU", y = "(%) GDP") +
  scale_fill_uchicago(name = "Sector") +  # Setting the title of the legend
  theme_bw()



### World graph ###
footprint <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/Teu.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

footprint <- as.data.table(rowSums(footprint))

label_IO <- as.data.table(s3read_using(FUN = readRDS,
                      object = paste(set_wd2,"/label_IO.rds",sep=""),
                      bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

footprint <- cbind(label_IO, footprint) %>%
  rename(score = V1)

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')


world_map <- map_data("world") %>% 
  filter(! long > 180)


match_pays_gloria_WM <- read_excel("data/match_pays_gloria_WM.xlsx", 
                                   sheet = "WM_Olson_gloria")


country_fp <- footprint %>%
  group_by(iso, country) %>%
  summarize(score=sum(score)) %>%
  distinct() %>%
  left_join(match_pays_gloria_WM, by=c("country"="pays_g"))


map <- world_map %>%
  left_join(country_fp,by=c("region"="WM"))

world_map1 <- world_map[which(map$iso %in% eu),]


map_ext <- map[which(!map$iso %in% eu),]
map_eu <- map[which(map$iso %in% eu),]


map_ext %>%
  ggplot(aes(map_id = region, fill = score)) +
  geom_map(map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               color = "black", fill = NA, size=0.1) +
  labs(fill = "nSTAR", fontface = "bold") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_map("moll") +
  theme(plot.title = element_text(hjust = 0.5)) 


map_eu %>%
  ggplot(aes(map_id = region, fill = score)) +
  geom_map(map = world_map1) +
  geom_polygon(data = world_map1, aes(x = long, y = lat, group = group),
               color = "black", fill = NA, size=0.1) +
  labs(fill = "nSTAR", fontface = "bold") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_map("moll") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(xmin = -15, xmax = 40) +  # Set longitude limits
  ylim(ymin = 30, ymax = 70)  



##### Graphs of country specific footprints #####
eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
                  "LUX", "POL", "PRT", "SVK")

top_12 <- top_12[which(top_12$variable %in% eu1),]

ggplot(top_12[which(top_12$variable == "ESP" & top_12$region == "eu"),],
       aes(x = sector, y = value)) +
  geom_bar(stat = "identity", color = "black") +  # Adding lines to each filled sector
  labs(x = "", y = "nSTAR") +
  #scale_fill_uchicago(name = "Sector") +  # Setting the title of the legend
  theme_bw() +
  coord_flip()



########### Pressure analysis ###########


Q_abs <- s3read_using(FUN = readRDS,
                      object = paste(set_wd3,"/Q_abs.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

label_Q <- as.data.frame(s3read_using(FUN = readRDS,
                      object = paste(set_wd3,"/label_Q.rds",sep=""),
                      bucket = bucket2, opts = list("region" = "")))

press <- label_Q[unique(Q_abs$Lfd_Nr),]

press <- press %>%
  mutate(Lfd_Nr = as.numeric(rownames(.))) %>%
  left_join(Q_abs, by = "Lfd_Nr")

s3write_using(x = as.data.table(press), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd3,"/press.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


View(press[press$country == "Austria" & press$sector == "Air transport", ])



top_12 <- as.data.frame(s3read_using(FUN = data.table::fread,
                       object = paste(set_wd2,"/top_12.rds",sep=""),
                       bucket = bucket2, opts = list("region" = "")))


View(top_12[top_12$variable == "FRA" & top_12$region == "ext",])
