##### Make some graphs ####

install.packages("viridis")  # Install
library("viridis")           # Load
install.packages("ggsci")
library("ggsci")


bucket1 = "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 = "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


k <- as.data.frame(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd2,"/k_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))


g <- k %>%
  group_by(V1, V2, eu) %>%
  mutate(tshare= abs(loss) / sum(output) * 100)


g <- g %>%
  group_by(V1, V2, eu) %>%
  arrange(desc(tshare)) %>%
  mutate(across(where(is.numeric), ~ ifelse(row_number() >= 6, sum(.), .))) %>%
  mutate(NACE = ifelse(row_number() == 6, "X", NACE)) %>%
  filter(row_number() <= 6)




ggplot(g, aes(x = eu, y = tshare, fill = factor(NACE))) +
  geom_bar(stat = "identity", color = "black") +  # Adding lines to each filled sector
  labs(x = "EU", y = "(%) GDP") +
  scale_fill_uchicago(name = "Sector") +  # Setting the title of the legend
  theme_bw()

