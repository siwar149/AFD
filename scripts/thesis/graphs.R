##### Make some graphs ####

#install.packages("viridis")  # Install
#library("viridis")           # Load
install.packages("ggsci")
library("ggsci")
install.packages("mapproj")
library("mapproj")
library(RColorBrewer)



bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


g <- s3read_using(FUN = data.table::fread,
                        object = paste(set_wd2,"/g3_2_2019.rds",sep=""),
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

n_s <- read_excel("data/NACE.xlsx", sheet = "Feuil1")

g1 <- g1 %>%
  left_join(n_s, by = c("NACE"="nace"))


ggplot(g1, aes(x = eu, y = revarx, fill = factor(sector))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +  # Adding transparency with alpha
  labs(x = "EU", y = "(%) Output") +
  scale_fill_uchicago(name = "") +  # Setting the title of the legend
  theme_bw() +
  theme(
    legend.position = "bottom"  # Move the legend to the bottom
  )


# Use the 'Set3' palette, which includes colors similar to the custom ones
brewer_palette <- brewer.pal(12, "Paired")
brewer_palette <- brewer_palette[c(4:12)]
custom_palette <- c("#7F7F7F", "#66C2A5", "#8DA0CB", "#A6D854", 
                    "#4D4D4D", "#B2E2E2", "#5E4FA2", "#5AAE61", "#E78AC3")


p <- ggplot(g1, aes(x = eu, y = revarx, fill = factor(sector))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(x = "", y = "(%) Output") +
  scale_fill_manual(values = brewer_palette, name = "") +  # Use the 'Set3' palette
  theme_bw() +
  theme(
    legend.position = "bottom",    # Position the legend at the bottom
    legend.key = element_blank(),  # Remove the legend keys (the little squares)
    legend.title = element_text(size = 12, face = "bold"),   # Customize legend title size and boldness
    legend.text = element_text(size = 10, face = "bold"),    # Customize legend text size and boldness
    axis.text = element_text(size = 12, face = "bold"),      # Make axis text larger and bold
    axis.title = element_text(size = 14, face = "bold"),     # Make axis title larger and bold
    axis.ticks = element_line(color = "black", size = 1),    # Make axis ticks thicker and darker
    panel.border = element_rect(color = "black", size = 2.3),# Thicker, darker panel border
    panel.grid.major = element_line(color = "grey80"),       # Optionally darken grid lines
    panel.grid.minor = element_blank()                       # Remove minor grid lines if you want a cleaner look
  )

ggsave(filename = "plots/1per_cent_shock.png", plot = p, width = 14, height = 8, dpi = 300)


### Financial graphs ###
fg <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/g3_3_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

fg <- fg %>%
  select(country, sector, abvarx, year, vaript)

anomalies <- which(fg$vaript < 0 | fg$vaript > 0.05)

bach <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd2,"/g3_3_2019.rds",sep=""),
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

label_IO <- as.data.table(s3read_using(FUN = readRDS,
                          object = paste(set_wd2,"/label_IO.rds",sep=""),
                          bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

cnts <- unique(label_IO$country)
isos <- unique(label_IO$iso)

iscnt <- as.data.table(cbind(isos, cnts))

fg <- fg %>%
  left_join(iso, by = c("country"="eu")) %>%
  left_join(iscnt, by = c("iso"="isos")) %>%
  filter(vaript != 0 | vaript == NA)

fg <- fg %>%
  left_join(n_s, by = c("sector"="nace")) %>%
  rename(NACE = sector.y)
  



# Create the bar plot with facets and custom fill scale
p <- ggplot(fg, aes(x = sector, y = vaript, fill = factor(NACE))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  facet_wrap(~ cnts) +  # Facet by country
  scale_fill_manual(values = rep("darkred", length(unique(fg$NACE))),  # Use a consistent color for NACE categories
                    labels = unique(fg$NACE),                     # Map NACE to sector names
                    name = "Sector") +                                # Legend title
  theme_bw() +
  labs(x = "Sector", y = "(%) var. ratio") +
  theme(
    legend.position = "bottom",         # Position the legend at the bottom
    legend.key = element_blank(),       # Remove the legend keys (the little squares)
    legend.title = element_blank(),  # Customize legend title size
    legend.text = element_text(size = 8)     # Customize legend text size
  )
  
# Save the plot as a PNG file
ggsave(filename = "plots/fin_exp_tot.png", plot = p, width = 15, height = 9, dpi = 300)




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


Teu1 <- Teu1 %>%
  left_join(n_s, by = c("NACE" = "nace"))

# Create the facet wrap plot

p <- ggplot(Teu1, aes(x = NACE, y = score, fill = factor(NACE))) +  # Use fill to differentiate NACE
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  scale_fill_manual(values = rep("navy", length(unique(Teu1$NACE))),  # Use a consistent color for NACE categories
                    labels = unique(Teu1$sector),                     # Map NACE to sector names
                    name = "Sector") +                                # Legend title
  labs(x = "Sectors", y = "nSTAR") +
  theme_bw() +
  theme(
    legend.position = "none",                # Eliminate the legend
    axis.title = element_text(size = 14, face = "bold"),  # Bold and larger axis titles
    axis.text = element_text(size = 12, face = "bold"),   # Bold and larger axis text
    strip.text = element_text(size = 14, face = "bold"),  # Bold and larger facet titles (headings)
    legend.key = element_blank(),            # Remove the legend keys (the little squares)
    legend.title = element_blank(),          # Remove legend title
    legend.text = element_text(size = 8)     # Customize legend text size (even though legend is removed)
  ) +
  facet_wrap(~ country, scales = "free", ncol = 3, nrow = 4)  # Facet by country, with 3 columns and 4 rows


# Save the plot to a file
ggsave(filename = "plots/exposure_ind.png", plot = p, width = 10, height = 12, dpi = 300)



### Exposure graph with Teu1
iso <- read_excel("data/iso.xlsx", sheet = "Sheet1")

g1 <- g1 %>% # go get this data from the financial script
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


p <- ggplot(Teu2, aes(x = country, y = exposure)) +
  geom_bar(stat = "identity", color = "black", fill = "navy", alpha = 0.7) +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    panel.border = element_rect(color = "black", size = 2),  # Thicker plot border
    axis.title = element_text(size = 14, face = "bold"),     # Larger and bold axis titles
    axis.text = element_text(size = 12, face = "bold")       # Larger and bold axis text
  ) +
  labs(x = "Countries", y = "(%) financial assets")


data <- data.frame(
  country = c("Germany", "France", "Spain", "Italy", "Belgium", "Poland", 
              "Portugal", "Austria", "Hungary", "Slovakia", "Luxembourg", "Croatia"),
  exposure = c(32.2, 41.9, 42.0, 50.5, 43.6, 32.1, 43.1, 48.5, 44.4, 34.5, 32.8, 38.1)
)


p <- ggplot(data, aes(x = country, y = exposure)) +
  # Bar for exposure
  geom_bar(stat = "identity", fill = "navy", color = "black") +
  # Transparent rectangle up to 100 with dashed border
  geom_rect(aes(xmin = as.numeric(factor(country)) - 0.4, xmax = as.numeric(factor(country)) + 0.4,
                ymin = exposure, ymax = 100),
            fill = NA, color = "black", linetype = "dashed") +
  coord_flip() +
  # Axis labels and plot title
  labs(x = "Country", y = "(%) National financial assets", title = "Exposure") +
  # Set y-axis limit
  ylim(0, 100) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", size = 2),  # Thicker plot border
    axis.title = element_text(size = 14, face = "bold"),     # Larger and bold axis titles
    axis.text = element_text(size = 12, face = "bold")       # Larger and bold axis text
  )

ggsave(filename = "plots/exposure_tot.png", plot = p, width = 6, height = 4, dpi = 300)




### World graph ###
footprint <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/Teu3.rds",sep=""),
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


p <- map_ext %>%
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
        axis.ticks = element_blank(),    # Remove tick marks from axes
        panel.grid = element_blank(),    # Remove coordinate lines
        plot.title = element_text(hjust = 0.5))  # Center plot title


ggsave(filename = "plots/Europes_footprint.png", plot = p, width = 12, height = 8, dpi = 300)


p <- map_eu %>%
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
        axis.ticks = element_blank(),    # Remove tick marks from axes
        panel.grid = element_blank(),     # Remove coordinate lines
        plot.title = element_text(hjust = 0.5)) +  # Center plot title
  xlim(xmin = -15, xmax = 40) +  # Set longitude limits
  ylim(ymin = 30, ymax = 70)   # Set latitude limits


ggsave(filename = "plots/Europes_destruction.png", plot = p, width = 12, height = 8, dpi = 300)



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
