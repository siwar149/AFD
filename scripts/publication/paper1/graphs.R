rm(list = ls())
gc()


install.packages("mapproj")
library("mapproj")


bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"



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

# Function to generate a random color gradient
generate_random_gradient <- function() {
  # Generate a random hue value for the gradient
  h1 <- runif(1, 0, 360)
  h2 <- runif(1, 0, 360)
  
  # Create a color gradient using the colorspace package
  colorspace::sequential_hcl(5, h = c(h1, h2))
}

# Loop through variables 9 to 18 in the "map" dataset
for (i in 9:18) {
  
  # Extract the variable name for the current index
  var_name <- names(map)[i]
  
  # Generate a random color gradient
  random_gradient <- generate_random_gradient()
  
  # Create the plot using ggplot2 with dynamic fill variable
  p <- map %>%
    ggplot(aes(map_id = region, fill = .data[[var_name]])) +
    geom_map(map = world_map) +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 color = "black", fill = NA, size = 0.1) +
    labs(fill = "nSTAR", title = var_name) +  # Add title using labs()
    scale_fill_gradientn(colours = random_gradient) +  # Use random color gradient
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




sfp <- s3read_using(FUN = data.table::fread,
                                   object = paste(set_wd3,"/sector_pressures.rds",sep=""),
                                   bucket = bucket2, opts = list("region" = ""))


sfp1 <- sfp %>%
  as.data.table() %>%
  cbind(label_IO, .data) %>%
  group_by(NACE) %>%
  summarise(V1 = sum(V1)) %>%
  mutate(shr = V1 / sum(V1) * 100) %>%
  arrange(desc(shr))

sfp1 <- sfp1[1:7, c(1,3)]
sfp1[7,1] <- "X"
sfp1[7,2] <- 100 - sum(sfp1[-7, 2])


sfp1 <- sfp1 %>%
  left_join(rp, by = "NACE")

for (i in c(3:5)) {

sfp1[7, i] <- 100 - sum(sfp1[-7, i])

}


# Reshape the data to long format
sfp1_long <- sfp1 %>%
  select(NACE, shr, efl, nh3, pl) %>%
  tidyr::pivot_longer(cols = c(shr, efl, nh3, pl), 
                      names_to = "variable", values_to = "value")

# Create a variable to assign each layer (outer ring for NACE, inner rings for variables)
sfp1_long <- sfp1_long %>%
  mutate(layer = case_when(
    variable == "shr" ~ 1,
    variable == "efl" ~ 2,
    variable == "nh3" ~ 3,
    variable == "pl" ~ 4
  ))


# Create the nested donut plot using the default color palette
plot <- ggplot(sfp1_long, aes(x = factor(layer), y = value, fill = NACE)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_x_discrete(expand = c(0, 0)) +
  theme_void() +
  labs(title = "") +
  theme(legend.position = "bottom",
        legend.title = element_blank())
  
# Add arrows and labels to point out each donut layer (variable)
plot <- plot +
  # Arrow for "shr"
  geom_segment(aes(x = 1.5, y = 80, xend = 1.5, yend = 45), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  annotate("text", x = 1.5, y = 85, label = "Sector", size = 5, fontface = "bold", color = "black") +
  
  # Arrow for "efl"
  geom_segment(aes(x = 2.5, y = 65, xend = 2.5, yend = 30), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  annotate("text", x = 2.5, y = 70, label = "Forestry", size = 5, fontface = "bold", color = "black") +
  
  # Arrow for "nh3"
  geom_segment(aes(x = 3.5, y = 50, xend = 3.5, yend = 15), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  annotate("text", x = 3.5, y = 55, label = "NH3", size = 5, fontface = "bold", color = "black") +
  
  # Arrow for "pl"
  geom_segment(aes(x = 4.5, y = 35, xend = 4.5, yend = 0), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  annotate("text", x = 4.5, y = 40, label = "Pastures", size = 5, fontface = "bold", color = "black")



ggsave(filename = paste0("plots/", "nested-donut", ".png"), plot = plot, width = 12, height = 8, dpi = 300)



# Arrange the data by 'shr' in descending order and calculate positions for labels
sfp1 <- sfp1 %>%
  arrange(desc(shr)) %>%
  mutate(
    fraction = shr / sum(shr),                      # Fraction of each section
    ymax = cumsum(fraction),                        # Cumulative sum for top of each section
    ymin = c(0, head(ymax, n = -1)),                # Start point for each section
    label_pos = (ymax + ymin) / 2,                  # Middle position for each section
    label = paste0(round(shr, 1), "%"),             # Create labels
    label_out = ifelse(shr < 5, "out", "in")        # Mark small portions for outside labels
  )

# Further increase the xlim to provide more space for labels
p <- p + xlim(0.5, 3.5)  # Further extended from 3 to 3.5 for more space outside the donut

# Separate labels for inside and outside positions
p <- p + geom_text(aes(x = 1.5, y = label_pos, label = label), color = "white", size = 5, 
                   data = filter(sfp1, label_out == "in"))  # Inside labels for larger slices

# Use ggrepel for outside labels for smaller portions
p <- p + geom_text_repel(aes(x = 3, y = label_pos, label = label), size = 5, 
                         data = filter(sfp1, label_out == "out"), nudge_x = 1, 
                         direction = "y", segment.size = 0.5, segment.color = "gray",
                         force = 0.5)  # Adjust nudge and force parameters for better positioning

# Show the plot
print(p)