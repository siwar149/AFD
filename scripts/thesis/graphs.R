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
                        object = paste(set_wd2,"/g1_2_2019.rds",sep=""),
                        bucket = bucket2, opts = list("region" = ""))


g0 <- g %>%
  group_by(iso, country, eu) %>%
  mutate(revarx= abs(dx) / sum(x) * 100)


g1 <- g0 %>%
  group_by(iso, country, eu) %>%
  arrange(desc(revarx)) %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(row_number() == 6, sum(.x[row_number() >= 6]), .x))) %>%
  mutate(NACE = ifelse(row_number() == 6, "X", NACE)) %>%
  filter(row_number() <= 6)


a <- g1 %>%
  group_by(iso, country, eu) %>%
  summarise(revarx = sum(revarx)) %>%
  pull(revarx)

ggplot(g1, aes(x = eu, y = revarx, fill = factor(NACE))) +
  geom_bar(stat = "identity", color = "black") +  # Adding lines to each filled sector
  labs(x = "EU", y = "(%) Output") +
  scale_fill_uchicago(name = "Sector") +  # Setting the title of the legend
  theme_bw()


### Financial graphs ###
fg <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/g1_3_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

fg <- fg %>%
  select(country, sector, abvarx, year, vaript)

anomalies <- which(fg$vaript < 0 | fg$vaript > 0.05)

bach <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd2,"/g1_3_2019.rds",sep=""),
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
  left_join(iscnt, by = c("iso"="isos")) %>%
  filter(vaript != 0 | vaript == NA)


# Create a list of unique countries
countries <- unique(fg$cnts)

# Loop through each country and create a bar plot
for (country in countries) {
  # Filter the data for the current country
  country_data <- fg %>% filter(cnts == !!country)
  
  # Create the bar plot
  p <- ggplot(country_data, aes(x = sector, y = vaript)) +
    geom_bar(stat = "identity", color = "black", fill = "darkred", alpha = 0.7) +
    theme_bw() +
    labs(
      title = country,  # Add country name as the title
      x = "Sector", 
      y = "(%) var. ratio"
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
  country_data <- Teu1 %>%
    filter(country == !!country_name)
  
  # Create the plot
  p <- ggplot(country_data, aes(x = NACE, y = score)) +
    geom_bar(stat = "identity", color = "black", fill = "navy", alpha = 0.7) +  # Add transparency here (0.7 for example)
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
  mutate(exposure = partexp / sumliab * 100) %>%
  distinct() %>%
  select(country, exposure)


ggplot(Teu2, aes(x = country, y = exposure)) +
  geom_bar(stat = "identity", color = "black", fill = "navy", alpha = 0.7) +
  theme_bw() +
  theme(plot.title = element_blank()) +
  labs(x = "Countries", y = "(%) financial liabilities")





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
               color = "black", fill = NA, size = 0.1) +
  labs(fill = "nSTAR", fontface = "bold") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_map("moll") +
  theme_bw() +
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove y-axis label
        axis.text.x = element_blank(),   # Remove x-axis values
        axis.text.y = element_blank(),   # Remove y-axis values
        panel.grid = element_blank(),    # Remove coordinate lines
        plot.title = element_text(hjust = 0.5))  # Center plot title


map_eu %>%
  ggplot(aes(map_id = region, fill = score)) +
  geom_map(map = world_map1) +
  geom_polygon(data = world_map1, aes(x = long, y = lat, group = group),
               color = "black", fill = NA, size = 0.1) +
  labs(fill = "nSTAR", fontface = "bold") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_map("moll") +
  theme_bw() +  # Set theme to black and white
  theme(axis.title.x = element_blank(),   # Remove x-axis label
        axis.title.y = element_blank(),   # Remove y-axis label
        axis.text.x = element_blank(),    # Remove x-axis values
        axis.text.y = element_blank(),    # Remove y-axis values
        panel.grid = element_blank(),     # Remove coordinate lines
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  xlim(xmin = -15, xmax = 40) +  # Set longitude limits
  ylim(ymin = 30, ymax = 70)   # Set latitude limits



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
install.packages("purrr")
library(purrr)


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

in_eu <- which(label_IO$iso %in% eu)

not_eu <- which(!label_IO$iso %in% eu)

pue <- Teu[in_eu,]
pext <- Teu[not_eu,]

# Define group size
group_size <- 120

# Calculate number of groups
num_groups <- ncol(pue) %/% group_size

# Initialize empty list to store results
pue_sum <- list()

# Loop through each group and calculate row sums
for (i in 0:(num_groups - 1)) {
  pue_sum[[i + 1]] <- pue[, rowSums(.SD, na.rm = TRUE), 
                      .SDcols = ((i * group_size + 1):((i + 1) * group_size))]
}

# Combine results into a single data.table
pue_sum <- as.data.table(do.call(cbind, pue_sum))


# for EXT
pext_sum <- list()

# Loop through each group and calculate row sums
for (i in 0:(num_groups - 1)) {
  pext_sum[[i + 1]] <- pext[, rowSums(.SD, na.rm = TRUE), 
                          .SDcols = ((i * group_size + 1):((i + 1) * group_size))]
}

# Combine results into a single data.table
pext_sum <- as.data.table(do.call(cbind, pext_sum))

colnames(pue_sum) <- eu1
colnames(pext_sum) <- eu1

pue <- colSums(pue_sum)
pext <- colSums(pext_sum)

tab <- data.table(eu1, pue, pext)

tab <- tab %>%
  mutate(share = pext / (pue + pext) * 100)

# Round all numeric columns to integers
tab[, (names(tab)) := lapply(.SD, function(x) if (is.numeric(x)) round(x) else x)]
