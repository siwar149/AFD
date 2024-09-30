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
