rm(list = ls())
gc()


install.packages("mapproj")
library(mapproj)
library(tidyr)
library(ggplot2)
library(WDI)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)
library(RColorBrewer)


# bucket1 <- "projet-esteem"
# set_wd1 <- "Gloria/matrices"
# 
# bucket2 <- "siwar"
# set_wd2 <- "data/Gloria"
# set_wd3 <- "data/bio/rds"


## destroying irwin
cns <- c("USA", "CHN", "JPN", "DEU", "FRA", "GBR",
         "LKA", "CIV", "BRA",
         "IDN", "PER", "MEX")

io <- as.data.table(s3read_using(FUN = readRDS,
                                 object = paste(set_wd2,"/label_IO.rds",sep=""),
                                 bucket = bucket2, opts = list("region" = "")))

colnames(io) <- c("iso", "country", "sector")

cns <- io %>%
  subset(iso %in% cns) %>%
  select(country) %>%
  unique()


results3f1 <- as.data.table(s3read_using(FUN = readRDS,
                    object = paste(set_wd3,"/results-v2.rds",sep=""),
                    bucket = bucket2, opts = list("region" = "")))


a <- results3f1[which(results3f1$country %in% cns$country),]
a <- a %>%
  mutate(exp = -exp) %>%
  rename(`D. Prod.` = prd,
         `Exp.` = exp,
         `Imp.` = imp,
         `N F.` = nfp)
  

long_data <- pivot_longer(a, cols = c(`D. Prod.`, `Exp.`, `Imp.`, `N F.`), 
                          names_to = "variable", values_to = "value")



# Create the facet wrap bar plots, faceting by country and grouped by 'type'
p <- ggplot(long_data, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +  # Create bar plots
  facet_wrap(~type + country, scales = "free", nrow = 6, ncol = 3) +  # Arrange into 6 rows and 3 columns
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(values = brewer.pal(4, "Dark2")) +  # Use the 'Dark2' color palette from RColorBrewer
  theme_bw() +  # Use theme_bw for a clean black-and-white theme
  theme(panel.border = element_rect(size = 2),
        legend.title = element_blank(),
        legend.position = "none")  # Make the panel borders thicker

ggsave(filename = "plots/irwin-comparison-v2.png", plot = p, width = 10, height = 12, dpi = 300)



### World maps of different pressures

results2_countries <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd3,"/pressures_countries.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))



world_map <- map_data("world") %>% 
  filter(! long > 180)


match_pays_gloria_WM <- read_excel("data/match_pays_gloria_WM.xlsx", 
                                   sheet = "WM_Olson_gloria")


world_fp <- results2_countries %>%
  distinct() %>%
  left_join(match_pays_gloria_WM, by=c("country"="pays_g"))


map <- world_map %>%
  left_join(world_fp,by=c("region"="WM"))




map[,8:13] <- log(map[,8:13])

# Loop through variables 9 to 18 in the "map" dataset
for (i in 8:13) {
  
  # Extract the variable name for the current index
  var_name <- names(map)[i]
  
  # Reverse the fixed color gradient to assign darkest color to highest values
  
  
  # Create the plot using ggplot2 with dynamic fill variable
  p <- map %>%
    ggplot(aes(map_id = region, fill = .data[[var_name]])) +
    geom_map(map = world_map) +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 color = "black", fill = NA, size = 0.1) +
    labs(fill = "nSTAR", title = "") +  # Add title using labs()
    scale_fill_gradient(low = "blue", high = "red") +  # Use reversed color gradient
    coord_map("moll") +
    theme_bw() +
    theme(axis.title.x = element_blank(),  # Remove x-axis label
          axis.title.y = element_blank(),  # Remove y-axis label
          axis.text.x = element_blank(),   # Remove x-axis values
          axis.text.y = element_blank(),   # Remove y-axis values
          axis.ticks = element_blank(),    # Remove tick marks from axes
          panel.grid = element_blank(),    # Remove coordinate lines
          panel.border = element_rect(color = "black", size = 2),  # Thicker plot frame
          plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title
  
  # Save the plot to the 'plots' directory with the variable name in the filename
  ggsave(filename = paste0("plots/", var_name, ".png"), plot = p, width = 12, height = 8, dpi = 300)
}






### Global net footprint of countries ###
library(scales)

results3f1 <- readRDS("rds/net-footprint-countries-v2.rds")

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% select(admin, adm0_a3,geometry)
world <- world %>% left_join(results3f1[,c(1,5,6)], by = c("adm0_a3"="country"))


world <- world %>%
  group_by(type) %>%
  mutate(NFP_scaled = rescale(nfp)) %>%
  ungroup()


# Create the plot
ggplot() +
  geom_sf(data = world,aes(fill = type, alpha = NFP_scaled)) +
  scale_alpha(range = c(0.4, 1), guide = "none") +  # hide alpha legend
  #coord_sf(crs = "+proj=robin") +
  coord_sf(xlim = c(-180, 200), ylim = c(-60, 80)) +
  scale_fill_viridis_d(name = "Footprint", option = "H", begin = 0.1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.background = element_rect(color = "black", size = 1),
        legend.spacing = unit(1, "cm"),
        legend.margin = margin(5, 5, 5, 5),
        plot.margin = margin(-3,-2,-3,-1,"cm")
  )# +
  #guides(fill = guide_legend(nrow = 2))


ggsave(plot = last_plot(), bg = "#ffffff",
       filename = "./plots/FIG2-world-net-footprint.png",
       width = 240, height = 140, units = "mm", scale = 1)


# Analyse specific countries
cns <- c('CHN', 'MDG', 'ARG', 'URY', 'PRY', 'NZL', 'TKM', 'UZB', 'GEO', 'MNG')

#CHN
a <- cbind(label_IO[label_IO$iso != cns[1],], s[which(label_IO$iso != cns[1]), cns[1]])
sort(a[,4],T)[1] / sum(a[,4]) # 10% from brazilian soy

# MDG
a <- cbind(label_IO[label_IO$iso == cns[2],], s[which(label_IO$iso == cns[2]), cns[2]])
sort(a[,4],T)[1] / sum(a[,4]) # 16% civil engineering   # 13% raising of cattle # 8% growing fruits and nuts

#  ARG
a <- cbind(label_IO[label_IO$iso == cns[3],], rowSums(s[which(label_IO$iso == cns[3]), colnames(s) != cns[3]]))
sort(a[,4],T)[1] / sum(a[,4]) # Exports of 36% raising of animals, ss to agri # 11% soy # 10% maize

#  URY
a <- cbind(label_IO[label_IO$iso == cns[4],], rowSums(s[which(label_IO$iso == cns[4]), colnames(s) != cns[4]]))
sort(a[,4],T)[1] / sum(a[,4]) # Exports of 38% forestry and logging # 18% leguminous crops # 7% raising cattle

# PRY
a <- cbind(label_IO[label_IO$iso == cns[5],], rowSums(s[which(label_IO$iso == cns[5]), colnames(s) != cns[5]]))
sort(a[,4],T)[1] / sum(a[,4]) # Exports of 36% leguminous crops # 9% cattle # 7% maize

#NZL 
a <- cbind(label_IO[label_IO$iso != cns[6],], s[which(label_IO$iso != cns[6]), cns[6]])
sort(a[,4],T)[1] / sum(a[,4]) # 10% from brazilian soy

# TKM
a <- cbind(label_IO[label_IO$iso == cns[7],], rowSums(s[which(label_IO$iso == cns[7]), colnames(s) != cns[7]]))
sort(a[,4],T)[1] / sum(a[,4]) # Exports of 19% fibre crops # 14% transport via pipeline # 11% distribution of gas through pipes

# UZB
a <- cbind(label_IO[label_IO$iso == cns[8],], rowSums(s[which(label_IO$iso == cns[8]), colnames(s) != cns[8]]))
sort(a[,4],T)[1] / sum(a[,4]) # Exports of 22% fibre crops # 15% fruits and nuts # 10% raising of animals, ss to agri




# net footprint of sectors barplot
sfp <- readRDS("rds/sector_pressures-1.rds")


sfp <- sfp %>%
  group_by(name) %>%
  arrange(desc(value)) %>%
  mutate(value = ifelse(row_number() == 7, sum(value[row_number() >= 7]), value)) %>%
  mutate(NACE = ifelse(row_number() == 7, "X", NACE)) %>%
  filter(row_number() <= 7) %>%
  ungroup()


n_s <- read_excel("data/NACE.xlsx", sheet = "Feuil1")

sfp <- sfp %>%
  left_join(n_s[,c(1,3)], by = c("NACE"="nace"))

biotope <- readRDS("matrices/biotope_threats.rds")
biotope[Lfd_Nr %in% c(68:72), pressure := paste0(pressure, " (Land use)")]
biotope[Lfd_Nr %in% c(80:84), pressure := paste0(pressure, " (PDF)")]

pressures <- biotope %>%
  select(-threat) %>%
  unique() %>%
  mutate(Lfd_Nr = as.character(Lfd_Nr))

sfp <- sfp %>% left_join(pressures, by = c('name'='Lfd_Nr'))

sfp[is.na(sfp)] <- 'Total STAR score'

sfp <- sfp %>% select(abbr, pressure, value)


# Just making a rotated barplot

# Create the horizontal barplot
sfp %>% filter(pressure != 'Pastures (PDF)') %>%
  ggplot(aes(x = pressure, y = value, fill = abbr)) +
  geom_bar(stat = "identity", position = position_fill(reverse = T)) +
  coord_flip() +  # This flips the plot to make it horizontal
  scale_fill_viridis_d(option = 'turbo', guide = guide_legend(ncol = 5, nrow = 3)) +
  #scale_fill_brewer(palette = "Dark2", guide = guide_legend(nrow = 4)) +  # Use a dark color palette
  labs(x = "Pressure", y = "(%)") +  # No plot title or legend title
  scale_y_continuous(labels = function(x) x * 100) +
  theme_minimal() +  # A cleaner, minimal theme with white background
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),  # White background
    plot.background = element_rect(fill = "white", colour = "white"),  # White plot background
    text = element_text(color = "black"),  # Text color in black
    axis.text = element_text(color = "black", size = 12, family = 'Computer Modern'),  # Axis values bold & larger
    axis.title = element_text(size = 12, family = 'Computer Modern'),  # Axis labels bold & larger
    legend.background = element_rect(fill = "white", colour = "white"),  # White legend background
    legend.text = element_text(color = "black", size = 12, family = 'Computer Modern'),  # Legend text bold & larger
    legend.title = element_blank(),  # Remove legend title
    legend.position = "bottom",
    plot.title = element_blank()  # Remove plot title
  )

ggplot2::ggsave(plot = last_plot(), bg = "#ffffff",
                filename = "./plots/FIG1-pressure-barplot.png",
                width = 240, height = 140, units = "mm", scale = 1)


# Show the plot
print(p)

