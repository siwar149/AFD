##### Make some graphs ####

bucket1 = "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 = "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


k <- as.data.frame(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd2,"/k6_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))


k0 <- k %>%
  group_by(V1, V2, eu) %>%
  mutate(tshare= abs(loss) / sum(x) * 100)

k1 <- k0 %>%
  group_by(V1, V2, eu) %>%
  arrange(desc(tshare)) %>%
  slice_head(n = 6)
  
ks <- k0 %>%
  group_by(eu) %>%
  summarise(
    tshare = sum(tshare)
  )

countries_to_eliminate <- c("DE", "HR", "PT")

# Filtering out the specified countries
k_filtered <- k1 %>%
  filter(!eu %in% countries_to_eliminate)



ggplot(k_filtered, aes(x = eu, y = share, fill = factor(NACE))) +
  geom_bar(stat = "identity") +
  labs(x = "EU", y = "Share") +
  scale_fill_discrete(name = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




