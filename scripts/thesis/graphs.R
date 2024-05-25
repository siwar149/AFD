##### Make some graphs ####

install.packages("viridis")  # Install
library("viridis")           # Load
install.packages("ggsci")
library("ggsci")
install.packages("mapproj")
library("mapproj")



bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
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
  mutate(across(where(is.numeric), 
                ~ ifelse(row_number() == 6, sum(.x[row_number() >= 6]), .x))) %>%
  mutate(NACE = ifelse(row_number() == 6, "X", NACE)) %>%
  filter(row_number() <= 6)




ggplot(g1, aes(x = eu, y = revarx, fill = factor(NACE))) +
  geom_bar(stat = "identity", color = "black") +  # Adding lines to each filled sector
  labs(x = "EU", y = "(%) GDP") +
  scale_fill_uchicago(name = "Sector") +  # Setting the title of the legend
  theme_bw()


### Financial graphs ###
fg <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/g_3_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

fg <- fg %>%
  select(country, sector, abvarx, year, vaript)

anomalies <- which(fg$vaript < 0 | fg$vaript > 0.05)

bach <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd2,"/g_3_2019.rds",sep=""),
                   bucket = bucket2, opts = list("region" = ""))

bach <- bach %>%
  select(country, sector, abvarx, year, I83_WM, I10_WM, I1_WM, R24_WM, ipt1, vaript)

bach <- bach[anomalies,]

bach <- bach %>%
  mutate(ipt0 = (I83_WM + I10_WM)*10^-2,
         vaript1 = (ipt1 - ipt0)/(ipt0)*100)

fg$vaript[anomalies] <- bach$vaript1 # fix negative values

summary(fg$vaript)

iso <- read_excel("data/iso.xlsx", sheet = "Sheet1")

cnts <- unique(label_IO$country)
isos <- unique(label_IO$iso)

iscnt <- as.data.table(cbind(isos, cnts))

fg <- fg %>%
  left_join(iso, by = c("country"="eu")) %>%
  left_join(iscnt, by = c("iso"="isos"))


# Create a list of unique countries
countries <- unique(fg$cnts)

# Loop through each country and create a bar plot
for (country in countries) {
  # Filter the data for the current country
  country_data <- fg %>% filter(cnts == !!country)
  
  # Create the bar plot
  p <- ggplot(country_data, aes(x = sector, y = vaript)) +
    geom_bar(stat = "identity", color = "black", fill = "darkred") +
    theme_bw() +
    labs(
      title = country,  # Add country name as the title
      x = "Sector", 
      y = "vaript"
    ) +
    theme(
      axis.text.x = element_text(size = 8),  # Remove rotation and set font size of x-axis labels
      axis.text.y = element_text(size = 8),  # Reduce font size of y-axis labels
      axis.title.x = element_text(size = 10),  # Reduce font size of x-axis title
      axis.title.y = element_text(size = 10),  # Reduce font size of y-axis title
      legend.position = "none"  # Remove the legend
    )
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("plots/vaript_by_sector_", country, ".png"), plot = p)
}



### Financial exposure by nSTAR footprint per country
Teu <- s3read_using(FUN = data.table::fread,
                    object = paste(set_wd2,"/Teu.rds",sep=""),
                    bucket = bucket2, opts = list("region" = ""))

label_IO <- s3read_using(FUN = data.table::fread,
                         object = "Gloria/labels/label_IO.rds",
                         bucket = bucket1, opts = list("region" = ""))

colnames(label_IO) <- c("iso", "country", "sector")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")


Teu1 <- as.data.table(colSums(Teu))

Teu1 <- cbind(label_IO[which(label_IO$iso %in% eu1),], Teu1)

colnames(Teu1)[5] <- "score"

Teu1 <- Teu1 %>%
  group_by(iso, country, NACE) %>%
  summarise(score = sum(score)) %>%
  arrange(desc(score)) %>%
  mutate(score = ifelse(row_number() == 8, sum(score[row_number() >= 8]), score)) %>%
  mutate(NACE = ifelse(row_number() == 8, "X", NACE)) %>%
  filter(row_number() <= 8) %>%
  distinct()

# Get the list of unique countries
countries <- unique(Teu1$country)

# Loop through each country, generate the plot, and save it
for (country_name in countries) {
  # Filter data for the current country
  country_data <- Teu1  %>%
    filter(country == !!country_name) %>%
    mutate(NACE = factor(NACE, levels = c(setdiff(unique(NACE[order(-score)]), "X"), "X")))
  
  # Create the plot
  p <- ggplot(country_data, aes(x = NACE, y = score)) +
    geom_bar(stat = "identity", color = "black", fill = "navy") +
    labs(title = country_name, x = "Sector", y = "nSTAR") +
    theme_bw()
  
  # Save the plot to a file
  ggsave(filename = paste0("plots/exposure", gsub(" ", "_", country_name), ".png"), plot = p)
}


### Exposure graph with Teu1
iso <- read_excel("data/iso.xlsx", sheet = "Sheet1")

g1 <- g1 %>%
  left_join(iso, by = c("country"="eu"))

Teu2 <- Teu1 %>%
  select(iso, NACE) %>%
  filter(NACE != "X") %>%
  rename(sector = NACE) %>%
  left_join(g1, by = c("iso", "sector")) %>%
  rename(country = country.y) %>%
  select(-country.x) %>%
  select(-iso) %>%
  group_by(country) %>%
  mutate(partexp = sum(liab)) %>%
  select(-liab) %>%
  select(-sector) %>%
  mutate(exposure = partexp / sumliab) %>%
  distinct() %>%
  select(country, exposure)






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


Teu <- s3read_using(FUN = data.table::fread,
             object = paste(set_wd2,"/Teu.rds",sep=""),
             bucket = bucket2, opts = list("region" = ""))

label_IO <- s3read_using(FUN = data.table::fread,
                               object = "Gloria/labels/label_IO.rds",
                               bucket = bucket1, opts = list("region" = ""))

colnames(label_IO) <- c("iso", "country", "sector")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

cs <- c("C", "F", "G", "H")

in_eu <- which(label_IO$iso %in% eu)

not_eu <- which(!label_IO$iso %in% eu)

label_Teu <- label_IO[which(label_IO$iso %in% eu1),]

cs_eu1 <- which(label_Teu$NACE %in% cs)

CS <- Teu[, ..cs_eu1]

CS_eu <- cbind(label_IO[in_eu,], CS[in_eu,])
CS_ext <- cbind(label_IO[not_eu,], CS[not_eu,])

rm(CS)

# get specific sectors
mcf <- s3read_using(FUN = data.table::fread,
                    object = paste(set_wd2,"/mcf.rds",sep=""),
                    bucket = bucket2, opts = list("region" = ""))

f <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/f_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

t <- mcf * f

tis <- t

tis[not_eu] <- 0

weights <- tis / sum(t)


t <- cbind(label_IO, t, weights)

colnames(t)[c(5,6)] <- c("score", "weight")

t <- t[which(t$iso %in% eu1 & t$NACE %in% cs),]

# 10 sectors with the most footprint
sec_analyse <- head(t[t$score > 2,] %>% arrange(desc(score)), 10)[, 1:5] # just change 10 if I want to see more sectors

label_Teu <- label_Teu[cs_eu1,]

label_Teu <- label_Teu %>%
  mutate(id = paste(iso, sector))

sec_analyse <- sec_analyse %>%
  mutate(id = paste(iso, sector))

ten <- which(label_Teu$id %in% sec_analyse$id)
ten1 <- ten + 4
ten1 <- c(c(1:4), ten1)

CS_eu <- CS_eu[, ..ten1]
CS_ext <- CS_ext[, ..ten1]

colnames(CS_eu)[c(5:14)] <- label_Teu$id[ten]
colnames(CS_ext)[c(5:14)] <- label_Teu$id[ten]


  #s3write_using(x = as.data.table(CS_eu), FUN = data.table::fwrite, na = "", 
  #              object = paste(set_wd2,"/CS_eu_2019.rds",sep=""),
  #              bucket = bucket2, opts = list("region" = ""))

#s3write_using(x = as.data.table(CS_ext), FUN = data.table::fwrite, na = "", 
#              object = paste(set_wd2,"/CS_ext_2019.rds",sep=""),
#              bucket = bucket2, opts = list("region" = ""))

CS_eu <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd2,"/CS_eu_2019.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

CS_ext <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd2,"/CS_ext_2019.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

colSums(CS_ext[, c(5:14)]) # we take Germany as an example






deu_alc_eu <- CS_eu[, c(1:4, 7)]
deu_alc_ext <- CS_ext[, c(1:4, 7)]

deu_alc_eu <- deu_alc_eu %>%
  arrange(desc(`DEU Alcoholic and other  beverages`)) %>%
  slice_head(n = 10)

deu_alc_ext <- deu_alc_ext %>%
  arrange(desc(`DEU Alcoholic and other  beverages`)) %>%
  slice_head(n = 10)

pressures <- s3read_using(FUN = readRDS,
             object = paste(set_wd3,"/redlist_score_per_pressure.rds",sep=""),
             bucket = bucket2, opts = list("region" = ""))


pressures <- pressures %>%
  mutate(id = paste(iso, sector))

deu_alc_eu <- deu_alc_eu %>%
  mutate(id=paste(iso, sector))

deu_alc_ext <- deu_alc_ext %>%
  mutate(id=paste(iso, sector))


pressures <- pressures %>%
  select(id, Lfd_Nr, score_sum)

deu_alc_eu <- deu_alc_eu %>%
  left_join(pressures, by = "id")


press <- s3read_using(FUN = data.table::fread,
                          object = paste(set_wd3,"/press.rds",sep=""),
                          bucket = bucket2, opts = list("region" = ""))

press <- press %>%
  mutate(id = paste(iso, sector)) %>%
  select(id, Lfd_Nr, Sat_head_indicator, Sat_unit, pressure)


deu_alc_eu <- deu_alc_eu %>%
  left_join(press, by = c("id", "Lfd_Nr"))

biotope <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd2,"/biotope_threats.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

colnames(biotope)[1] <- "sat"

deu_alc_eu <- deu_alc_eu %>%
  left_join(biotope, by = "Lfd_Nr")


deu_alc_ext <- deu_alc_ext %>%
  left_join(pressures, by = "id")

deu_alc_ext <- deu_alc_ext %>%
  left_join(press, by = c("id", "Lfd_Nr"))

deu_alc_ext <- deu_alc_ext %>%
  left_join(biotope, by = "Lfd_Nr")


deu_alc_eu <- deu_alc_eu %>%
  select(iso, country, sector, NACE, `DEU Alcoholic and other  beverages`, Sat_head_indicator,
         sat, Sat_unit, pressure, score_sum, threat)

deu_alc_ext <- deu_alc_ext %>%
  select(iso, country, sector, NACE, `DEU Alcoholic and other  beverages`, Sat_head_indicator,
         sat, Sat_unit, pressure, score_sum, threat)

View(deu_alc_ext[deu_alc_ext$iso == "PER",])
