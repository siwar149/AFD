rm(list = ls())
gc()


install.packages("mapproj")
library("mapproj")
library("tidyr")


bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


## destroying irwin
cns <- c("USA", "CHN", "JPN", "DEU", "FRA", "GBR",
         "LKA", "CIV", "BRA",
         "IDN", "PER", "MEX")


cns <- label_IO %>%
  subset(iso %in% cns) %>%
  select(country) %>%
  unique()


a <- results3f1[which(results3f1$country %in% cns$country),]
a <- a %>%
  mutate(fexp = -fexp) %>%
  rename(`Dom.` = fdom,
         `Exp.` = fexp,
         `Imp.` = fimp,
         `N F.` = nfp)
  

long_data <- pivot_longer(a, cols = c(`Dom.`, `Exp.`, `Imp.`, `N F.`), 
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

ggsave(filename = "plots/irwin-comparison.png", plot = p, width = 10, height = 12, dpi = 300)



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



# Install necessary package if not already installed
if (!requireNamespace("colorspace", quietly = TRUE)) {
  install.packages("colorspace")
}

# Define a fixed color gradient from white to purple to dark blue
fixed_gradient <- colorspace::sequential_hcl(5, h = c(270, 250), l = c(100, 40, 10), power = 1.2)


map <- map %>%
  rename(`CO2 exc. short cycle org. carbon` = CO2_excl_short_cycle_org_c,
         `CO2 org. short cycle carbon` = CO2_org_short_cycle_c)

# Loop through variables 9 to 18 in the "map" dataset
for (i in 9:18) {
  
  # Extract the variable name for the current index
  var_name <- names(map)[i]
  
  # Reverse the fixed color gradient to assign darkest color to highest values
  
  
  # Create the plot using ggplot2 with dynamic fill variable
  p <- map %>%
    ggplot(aes(map_id = region, fill = .data[[var_name]])) +
    geom_map(map = world_map) +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 color = "black", fill = NA, size = 0.1) +
    labs(fill = "nSTAR", title = var_name) +  # Add title using labs()
    scale_fill_gradientn(colours = fixed_gradient) +  # Use reversed color gradient
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
library(RColorBrewer)

results3f1 <- s3read_using(FUN = data.table::fread,
                    object = paste(set_wd3,"/net-footprint-countries.rds",sep=""),
                    bucket = bucket2, opts = list("region" = ""))

world_nfp <- results3f1 %>%
  distinct() %>%
  left_join(match_pays_gloria_WM, by=c("country"="pays_g"))


map <- world_map %>%
  left_join(world_nfp,by=c("region"="WM"))


# Create the plot
# Replace NA values in nfp with 0
map <- map %>%
  mutate(nfp = replace_na(nfp, 0))  # Replace NAs with 0


# Create the plot
p <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA, size = 0.1) +
  
  # Fill according to type and grade by nfp
  geom_map(data = map, aes(map_id = region, fill = type, alpha = nfp), 
           map = world_map) +
  
  # Define colors for each type
  scale_fill_manual(values = c("Net Importer" = "red", "Net Domestic Consumer" = "blue"), 
                    guide = "legend") +  # Keep the legend
  # Adjust alpha for gradient effect based on nfp
  scale_alpha_continuous(range = c(0.3, 1), 
                         guide = "none") +  # Adjust alpha range as needed
  
  coord_map("moll") +
  labs(fill = NULL, alpha = "nSTAR") +  # Remove fill legend title
  
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),  # Thicker plot frame
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",   # Position legend at the bottom
        legend.title = element_blank(), # Remove the legend title
        legend.key.size = unit(1, "cm"))  # Adjust legend key size if needed

# Display the plot
print(p)

ggsave(filename = "plots/world-net-footprint.png", plot = p, width = 12, height = 8, dpi = 300)




# net footprint of sectors barplot
sfp <- s3read_using(FUN = data.table::fread,
                     object = paste(set_wd3,"/sector_pressures-1.rds",sep=""),
                     bucket = bucket2, opts = list("region" = ""))


sfp1 <- sfp %>%
  as.data.table() %>%
  mutate(shr = nfp / sum(nfp) * 100) %>%
  select(sector, nfp, shr) %>%
  arrange(desc(shr)) %>%
  rename(`Total Net Fooprint (nSTAR)` = shr)

sfp1 <- sfp1[1:7, c(1,3)]
sfp1[7,1] <- "X"
sfp1[7,2] <- 100 - sum(sfp1[-7, 2])


results2_pressures <- s3read_using(FUN = data.table::fread,
                     object = paste(set_wd3,"/pressures_per_sector.rds",sep=""),
                     bucket = bucket2, opts = list("region" = ""))

rp <- results2_pressures %>%
  mutate(across(where(is.numeric), ~ . / sum(.) * 100))

sfp1 <- sfp1 %>%
  left_join(rp, by = c("sector"="NACE")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 100 - sum(., na.rm = TRUE), .)))


# Just making a rotated barplot

# Use pivot_longer to reshape the dataset to long format
sfp1_long <- sfp1 %>%
  pivot_longer(cols = -sector, names_to = "variable", values_to = "value")

# Create the horizontal barplot
p <- ggplot(sfp1_long, aes(x = variable, y = value, fill = sector)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # This flips the plot to make it horizontal
  scale_fill_brewer(palette = "Dark2") +  # Use a dark color palette
  labs(x = "", y = "Value") +  # No plot title or legend title
  theme_minimal() +  # A cleaner, minimal theme with white background
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),  # White background
    plot.background = element_rect(fill = "white", colour = "white"),  # White plot background
    text = element_text(color = "black"),  # Text color in black
    axis.text = element_text(color = "black", size = 14, face = "bold"),  # Axis values bold & larger
    axis.title = element_text(size = 16, face = "bold"),  # Axis labels bold & larger
    legend.background = element_rect(fill = "white", colour = "white"),  # White legend background
    legend.text = element_text(color = "black", size = 12, face = "bold"),  # Legend text bold & larger
    legend.title = element_blank(),  # Remove legend title
    plot.title = element_blank()  # Remove plot title
  )

ggsave(filename = "plots/pressure-barplot.png", plot = p, width = 12, height = 8, dpi = 300)


# Show the plot
print(p)

