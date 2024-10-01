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



p <- map %>%
  ggplot(aes(map_id = region, fill = `Pastures (Land use)`)) +
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

print(p)




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